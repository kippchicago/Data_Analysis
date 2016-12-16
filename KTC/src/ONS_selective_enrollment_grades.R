require(tufte)
require(dplyr)
require(lubridate)
require(silounloadr)
require(ggplot2)
require(stringr)
require(tidyr)


grades <- get_ps('storedgrades')
students <- get_ps('students')


termid <- calc_ps_termid(2015)


grades_7 <- grades %>% filter(TERMID == termid,
                              STORECODE == "Y1",
                              GRADE_LEVEL==7) %>%
  inner_join(students %>%
               filter(ENROLL_STATUS == 0) %>%
               select(ID, STUDENT_NUMBER, FIRST_NAME, LAST_NAME, DOB, GENDER),
             by = c("STUDENTID" = "ID")) %>%
  collect() %>%
  select(STUDENT_NUMBER, LAST_NAME, FIRST_NAME, DOB,GENDER, SCHOOLID, SCHOOLNAME, COURSE_NAME, COURSE_NUMBER, GRADE, GPA_POINTS, ABSENCES, TARDIES) %>%
  mutate(DOB = ymd_hms(DOB),
         gpa_points = ifelse(GRADE=="--" | is.na(GRADE), NA, GPA_POINTS))

academic_grades <- grades_7 %>%
  filter(grepl("ela|math|ss|sci", COURSE_NUMBER)) %>%
  select(studentid = STUDENT_NUMBER,
         COURSE_NUMBER,
         COURSE_NAME,
         GRADE,
         gpa_points)

names(academic_grades) <- tolower(names(academic_grades))

se_points <- data_frame(se_grade = c("A", "B", "C", "D", "F"),
                        se_points = c(75, 50, 25, 0, 0))

academic_grades_points <-academic_grades %>%
  mutate(se_grade = str_extract(grade, "[ABCDF]")) %>%
  inner_join(se_points, by="se_grade")

se_grades_spread <- academic_grades_points %>%
  mutate(course_abbrev = str_extract(course_number, "ela|math|ss|sci")) %>%
  select(studentid, course_abbrev, se_grade) %>%
  spread(course_abbrev, se_grade)

se_grades_report <- attend_demo %>%
  inner_join(se_grades_spread, by="studentid") %>%
  mutate(school = abbreviate(schoolname)) %>%
  arrange(school, last_name, first_name, studentid)

readr::write_csv(se_grades_report %>% filter(school == "KAMS"),
                 "../reports/selective_enrollment_grades_KAMS.csv")

readr::write_csv(se_grades_report %>% filter(school == "KCCP"),
                 "../reports/selective_enrollment_grades_KCCP.csv")

readr::write_csv(se_grades_report %>% filter(school == "KBCP"),
                 "../reports/selective_enrollment_grades_KBCP.csv")



