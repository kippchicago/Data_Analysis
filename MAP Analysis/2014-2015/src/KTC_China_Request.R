# CSV of KIPP Create rising 8th graders (Spring 2015 7th graders) for China Hill ###

load("data/map_mv.RDA")

require(mapvizieR)
require(dplyr)
require(tidyr)
require(lubridate)

# Compile MAP data ####
# 1. filter to KCCP, Grade 7 in 2014-2015
map_mv_filtered<-mv_filter(map_mv,
                           roster_filter = quote(school=='KCCP' &
                                                      grade==7 &
                                                      map_year_academic==2014 &
                                                 termname == "Spring 2014-2015"),
                           cdf_filter = quote(measurementscale %in% c('Mathematics', 'Reading'))

          )

# 2. Combine roster and
map_combined <- map_mv_filtered$roster %>%
  select(studentid, studentfirstname, studentlastname, studentdateofbirth, school) %>%
  as.data.frame %>%
  unique() %>%
  arrange(studentid) %>%
  inner_join(map_mv_filtered$cdf, by="studentid") %>%
  select(studentid:studentdateofbirth,
         measurementscale,
         grade_season_label,
         testritscore,
         testpercentile)


map_combined <- unique(map_combined)

# recast

map_spread_t_RIT<-map_combined %>%
  select(studentid:grade_season_label, testritscore) %>%
  spread(key=grade_season_label, value=testritscore)


# pull apart subjects

rit_scores<-  map_spread_t_RIT %>%
    filter(measurementscale=="Reading") %>%
    select(-measurementscale) %>%
    inner_join(map_spread_t_RIT %>%
                 filter(measurementscale == "Mathematics") %>%
                 select(studentid, matches("[0-9][F|W|S]")
                        ),
               by="studentid"
               )

rit_col_names <-names(rit_scores)

names(rit_scores) <- gsub("\\.x", "_reading", rit_col_names) %>%
                     gsub("\\.y", "_math", .)

# now for percentiles

map_spread_t_pctl<-map_combined %>%
  select(studentid:grade_season_label, testpercentile) %>%
  spread(key=grade_season_label, value=testpercentile)

rit_pctls<-  map_spread_t_pctl %>%
  filter(measurementscale=="Reading") %>%
  select(-measurementscale) %>%
  inner_join(map_spread_t_pctl %>%
               filter(measurementscale == "Mathematics") %>%
               select(studentid, matches("[0-9][F|W|S]")
               ),
             by="studentid"
  )

pctl_col_names <-names(rit_pctls)

names(rit_pctls) <- gsub("\\.x", "_reading_pctl", rit_col_names) %>%
                     gsub("\\.y", "_math_pctl", .)


# recombine scores and percentiles
rit_scores_pctls<-rit_scores %>%
  inner_join(rit_pctls %>%
               select(studentid, contains("_pctl")),
             by="studentid") %>%
  select(studentfirstname,
         studentlastname,
         studentdateofbirth,
         studentid,
         contains("7S_math"),
         contains("7W_math"),
         contains("7F_math"),
         contains("6S_math"),
         contains("6W_math"),
         contains("6F_math"),
         contains("5S_math"),
         contains("5F_math"),
         contains("7S_reading"),
         contains("7W_reading"),
         contains("7F_reading"),
         contains("6S_reading"),
         contains("6W_reading"),
         contains("6F_reading"),
         contains("5S_reading"),
         contains("5F_reading")
         ) %>%
  arrange(studentlastname, studentfirstname)

# Get student Attendance ####
silo<-as.data.frame(read.dcf('config//silo_dw.dcf'))

drvr <- JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver',
             '~/Dropbox (KIPP Chicago Schools)/JDBC Drivers/sqljdbc_4.0/enu//sqljdbc4.jar')

conn <- dbConnect(drvr,
                  databaseName=as.character(silo$dbname),
                  url=as.character(silo$url),
                  user=as.character(silo$user),
                  password=as.character(silo$password)
)


# get current attendance
qry<-"SELECT * FROM [PS_mirror].[dbo].[attendance]"
current_attendance<-dbGetQuery(conn, qry)

qry<-"SELECT * FROM [PS_mirror].[dbo].[membership]"
current_membership<-dbGetQuery(conn, qry)


attend <- current_membership %>%
  mutate(date = ymd_hms(CALENDARDATE)) %>%
  left_join(current_attendance %>%
              mutate(date = ymd_hms(ATT_DATE)),
            by=c("STUDENTID", "date")) %>%
  inner_join(rit_scores_pctls %>% select(studentid) %>%
               mutate(studentid = as.numeric(studentid)),
             by = c("STUDENT_NUMBER" = "studentid")) %>%
  mutate(present = PRESENCE_STATUS_CD=="Present",
         present = ifelse(is.na(present), TRUE, present),
         sy=1991+YEARID) %>%
  rename(studentid = STUDENT_NUMBER)

attend_by_stu_by_yr <- attend %>%
  group_by(studentid, sy) %>%
  summarize(enrolled = n(),
            present = sum(present),
            ADA = round(present/enrolled*100,1))

ada_by_stu <- attend_by_stu_by_yr %>%
  select(studentid, sy, ADA) %>%
  spread(sy, ADA)

out <- left_join(rit_scores_pctls %>% mutate(studentid = as.numeric(studentid)),
                 ada_by_stu,
                 by="studentid")
# combine MAP with
# Write csv
write.csv(out, "reports/KTC_China_request.csv", na = "")


