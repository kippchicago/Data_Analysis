# Connect to server using JDBC Connection (note: requires a VPN connection to be open to psshostingvpn.poowerschool.com)
require(RJDBC)
require(dplyr)
require(googlesheets)
require(ggplot2)
require(tidyr)

drvr <- JDBC("oracle.jdbc.driver.OracleDriver", "/Users/chaid/Dropbox/JDBC Drivers/ojdbc6.jar","") # define driver

pspw <- as.list(read.dcf("config/ps.dcf", all=TRUE)) #read DCF with configuration settings

pscon <- dbConnect(drvr,pspw$SERVER,pspw$UID,pspw$PWD) # connect to server

#
sql.statement<-paste("
SELECT
        s.*,
        grades.SCHOOLID,
        grades.DATEENROLLED,
        grades.dateleft,
        grades.termid,
        grades.section_number,
        grades.course_number,
        grades.finalgradename,
        grades.grade,
        grades.percent
FROM
        (SELECT
                 cc.* ,
                 pgf.finalgradename,
                 pgf.grade,
                 pgf.percent
                 FROM        pgfinalgrades pgf
                 LEFT JOIN   cc
                 ON cc.sectionid = pgf.sectionid AND cc.studentID= pgf.studentID
                 WHERE cc.DATEENROLLED >='01-AUG-15'
                 --AND cc.schoolid=7810
                ) grades
LEFT JOIN  (SELECT
                    ID AS studentid,
                    student_number,
                    lastfirst,
                    grade_level,
                    home_room
                    FROM
                    students
                    WHERE enroll_status=0
                    ) s
ON s.studentID = grades.studentid",
                     sep="")

#Execture qurey and return to data frame
grades<-dbGetQuery(pscon, sql.statement)


failing_students<-grades %>%
  filter(FINALGRADENAME == "Y1",
         PERCENT <= 70,
         GRADE != "--") %>%
  group_by(SCHOOLID, STUDENT_NUMBER, LASTFIRST, GRADE_LEVEL) %>%
  summarise(n_classes_failing =n(),
            class_list =  paste(COURSE_NUMBER, collapse = ", "),
            percent_list = paste(PERCENT, collapse = ", ")) %>%
  ungroup() %>%
  arrange(SCHOOLID, GRADE_LEVEL, desc(n_classes_failing), LASTFIRST) %>%
  select(schoolid = SCHOOLID,
         "Student Number" = STUDENT_NUMBER,
         "Name" = LASTFIRST,
         Grade = GRADE_LEVEL,
         "Number of failing classes" = n_classes_failing,
         "Classes failing" = class_list,
         "Failing grades" = percent_list)

failing_students

fg_summary <- grades %>%
  filter(FINALGRADENAME == "Y1",
         GRADE != "--") %>%
  group_by(SCHOOLID, STUDENT_NUMBER, LASTFIRST, GRADE_LEVEL) %>%
  summarise(n_classes_failing = sum(PERCENT<=70)) %>%
  mutate(failing_1 = n_classes_failing >= 1,
         failing_2 = n_classes_failing >= 2,
         failing_3 = n_classes_failing >= 3) %>%
  group_by(SCHOOLID, GRADE_LEVEL) %>%
  summarize(n_students = n(),
            n_failing_1 = sum(failing_1),
            n_failing_2 = sum(failing_2),
            n_failing_3 = sum(failing_3),
            pct_failing_1 = round(n_failing_1 / n_students * 100),
            pct_failing_2 = round(n_failing_2 / n_students * 100),
            pct_failing_3 = round(n_failing_3 / n_students * 100)
  )

fg_summary

sheet_name <- "15-16 Q2 Failing Students - Updated 16.02.05"
if(is.null(gs_ls(sheet_name))) {
  gs_fs<-gs_new(sheet_name)

  gs_fs %>%
    gs_ws_new("Summary", input=fg_summary) %>%
    gs_ws_new("KAMS",
              input = failing_students %>%
                filter(schoolid==7810),
              trim = TRUE) %>%
    gs_ws_new("KCCP",
              input = failing_students %>%
                filter(schoolid==400146),
              trim = TRUE) %>%
    gs_ws_new("KBCP",
              input = failing_students %>%
                filter(schoolid==400163),
              trim = TRUE) %>%
    gs_ws_delete("Sheet1")
} else {

  gs_fs <- gs_title(sheet_name)
  gs_fs %>%
    #gs_ws_delete("Summary") %>%
    gs_ws_delete("KAMS") %>%
    gs_ws_delete("KCCP") %>%
    gs_ws_delete("KBCP") %>%
    gs_ws_new("Summary", input=fs_summary) %>%
    gs_ws_new("KAMS",
              input = failing_students %>%
                filter(schoolid==7810),
              trim = TRUE) %>%
    gs_ws_new("KCCP",
              input = failing_students %>%
                filter(schoolid==400146),
              trim = TRUE) %>%
    gs_ws_new("KBCP",
              input = failing_students %>%
                filter(schoolid==400163),
              trim = TRUE)

}


# Grade-subject failure reates level failures

kams_pct_Fs<-grades %>%
  filter(FINALGRADENAME == "Y1",
         GRADE != "--") %>%
  group_by(SCHOOLID, GRADE_LEVEL, COURSE_NUMBER) %>%
  summarise(n_students=n(),
            n_Fs = sum(GRADE=="F"),
            pct_Fs = round(100*n_Fs/n_students)) %>%
  filter(SCHOOLID  == 7810)

gs_new("KAMS_F_Summary")

gs_fs_KAMS <- gs_new("KAMS_F_Summary")

gs_fs_KAMS %>%
  gs_ws_new("KAMS % Fs", input=kams_pct_Fs)

