require(RJDBC)
require(dplyr)
require(RSQLServer)
require(ggplot2)
require(tidyr)
require(lubridate)
require(stringr)
require(shiny)
require(miniUI)


# get attendance data
silo<-as.data.frame(read.dcf('config//silo_dw.dcf'))

# drvr <- JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver',
             # '~/Dropbox (KIPP Chicago Schools)/JDBC Drivers/sqljdbc_4.0/enu//sqljdbc4.jar')

silo_ps_db <- src_sqlserver(server =  silo$url,
                            database = silo$dbname,
                            properties = list(user = silo$user,
                                              password = silo$password))



# Get all the tables we need!

pg_final_grades <- tbl(silo_ps_db, "pgfinalgrades")

pg_assignments <- tbl(silo_ps_db, "pgassignments")

students <- tbl(silo_ps_db, sql("SELECT * FROM students WHERE enroll_status = 0"))

section_scores_assignments <- tbl(silo_ps_db, "SectionScoresAssignments")

section_scores_ids <- tbl(silo_ps_db, "SectionScoresID")

sections <- tbl(silo_ps_db, sql("SELECT * from sections where TERMID = 2500"))
sections <- tbl(silo_ps_db, "sections")

cc <- tbl(silo_ps_db, sql("SELECT * FROM cc WHERE DATEENROLLED >='01-AUG-15'"))


pg_fg_cc <- pg_final_grades %>%
  select(SECTIONID,
         STUDENTID,
         FINALGRADENAME,
         GRADE,
         PERCENT) %>%
  left_join(cc, by = c("SECTIONID", "STUDENTID")) %>%
  rename(SECTIONID= SECTIONID.x,
         STUDENTID = STUDENTID.x) %>%
  select(-ends_with(".y"))

grades_1<-pg_fg_cc %>%
  left_join(students %>%
              select(STUDENTID = ID,
                     STUDENT_NUMBER,
                     LASTFIRST,
                     GRADE_LEVEL,
                     HOME_ROOM
                     ),
            by = "STUDENTID") %>%
  collect()

grades <- grades_1 %>%
  select( STUDENTID = STUDENTID.x,
          LASTFIRST,
          GRADE_LEVEL,
          HOME_ROOM,
          SCHOOLID,
          DATEENROLLED,
          DATELEFT,
          TERMID,
          SECTION_NUMBER,
          COURSE_NUMBER,
          FINALGRADENAME,
          GRADE,
          PERCENT)

names(grades) <- tolower(names(grades))

# assignments


assignments_1<-
  section_scores_ids %>%
  select(DCID, SECTIONID, STUDENTID) %>%
  inner_join(section_scores_assignments, by = c("DCID" = "FDCID")) %>%
  left_join(sections %>%
              select(ID,
                     GRADE_LEVEL,
                     COURSE_NUMBER,
                     TEACHER,
                     TERMID), by = c("SECTIONID" = "ID")) %>%
  left_join(pg_assignments, by = c("ASSIGNMENT" = "ID",
                                   "SECTIONID" = "SECTIONID"))

assignments <- collect(assignments_1)


names(assignments) <- tolower(names(assignments))


students_1 <- collect(students)
names(students_1) <- tolower(names(students_1))



# join student data to assignment data ####
assignments_students <- assignments %>%
  inner_join(students_1 %>%
               select(-starts_with("DCID")) %>%
               rename(studentid = id), by = "studentid")

grade_order <- rev(c("A+", "A", "A-", "B+", "B", "B-", "C+", "C", "C-", "F"))
grade_cols <- scales::brewer_pal("div", "RdYlGn")(length(grade_order))

grade_scale <- data_frame(grade_order, grade_cols)

q3 <- interval(mdy("04/04/2016"), mdy("06/15/2016"))


quarter_dates <- as.POSIXlt(c("2015-08-15",
                              "2015-10-31",
                              "2016-01-31",
                              "2016-04-04",
                              "2016-06-16"))

quarters <- c("Q1", "Q2", "Q3", "Q4")

# Calculate summary data from assignments
calc_cums <-. %>%
  mutate(date = ymd_hms(datedue),
         quarter = cut.POSIXt(date, breaks = quarter_dates, labels = quarters),
         percent = as.double(percent),
         score = as.double(score),
         exempt = as.logical(as.integer(exempt)),
         includeinfinalgrades = as.logical(includeinfinalgrades),
         grade = factor(as.character(grade), levels = grade_order, ordered = TRUE)) %>%
  filter(!is.na(percent),
         !is.na(quarter)
         ) %>%
  group_by(studentid, lastfirst, course_number, quarter) %>%
  mutate(cum_mean_score = order_by(date, cummean(score)),
         cum_mean_percent = order_by(date, cummean(percent)),
         weighted_points = weight*score,
         weighted_points_possible = weight*pointspossible,
         weighted_percent = weighted_points/weighted_points_possible,
         cum_weighted_points_possible =order_by(date, cumsum(weighted_points_possible)),
         cum_weighted_points = order_by(date, cumsum(weighted_points)),
         cum_weighted_avg = cum_weighted_points/cum_weighted_points_possible,
         cum_grade = cut(cum_weighted_avg, ordered_result = TRUE,
                         breaks= c(0,.69,.72,.76,.79,.82,.84,.89,.93,.97, 100.00),
                         labels =  grade_order)
         ) %>%
  filter(n()>5) %>%
  inner_join(grade_scale, by = c("grade" = "grade_order"))


assignments_2 <- calc_cums(assignments_students)

assignments_2 %>%
  glimpse

assignments_max_dates <- assignments_2 %>%
  filter(date==max(date) & assignmentid == max(assignmentid)) %>%
  select(-grade_cols) %>%
  inner_join(grade_scale, by = c("cum_grade" = "grade_order"))


ggplot(assignments_2 %>%
         filter(course_number == "kccp8math", quarter=="Q4"),
       aes(x=date, y=weighted_percent)) +
  geom_text(data = assignments_max_dates %>%
              filter(course_number == "kccp8math", quarter=="Q4"),
            aes(y=0, x= ymd("2016-4-15"),
                label=round(cum_weighted_avg*100),
                color=grade_cols
            ),
            alpha = .5,
            size = 12,
            vjust = 0,
            show.legend =FALSE) +
  geom_line(aes(group=studentid, y=cum_weighted_avg)) +
  geom_point(aes(color=grade_cols, size = weighted_points_possible, shape = exempt)) +
  facet_wrap(~lastfirst) +
  scale_color_identity("Grades",
                       labels = grade_scale$grade_order,
                       breaks = grade_scale$grade_cols,
                       guide = "legend") +
  #scale_color_brewer(palette = "RdYlGn", direction=-1) +
  scale_shape_manual(values=c(16,21)) +
  theme_bw() +
  labs(title = "title_text",
       x = "Date",
       y = "Percent",
       color = "Grade",
       shape = "Exempt",
       size = "Relative Weight")
