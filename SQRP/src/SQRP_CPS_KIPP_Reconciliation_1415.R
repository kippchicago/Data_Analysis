# SQRP CSP vs KIPP Chicago Reconciliation
# NWEA MAP Munging Script

# rbind all NWEA MAP tables from CPS into one table

files<-list.files("~/Dropbox (KIPP Chicago Schools)/Data Analysis/SQRP/data/",
           pattern="SY15", full.names = TRUE)


map_cps_list <- lapply(files, read.csv)

map_cps<-rbind_all(map_cps_list)

map_cps$School<-mapply(switch, as.character(map_cps$Student.s.Annualized.School.ID),
       MoreArgs = list("400044"="Ascend",
                       "400146"="KCCP",
                       "400163"="KBCP")
)

map_cps$StudentID <- map_cps$Student.ID

map_kipp<-filter(map_all_silo_2,
                 TermName %in% c("Spring 2013-2014", "Spring 2014-2015"),
                 MeasurementScale %in% c("Mathematics", "Reading"),
                 Grade>=2
                 ) %>%
  mutate(DistrictName = "KIPP Chicago")



data(ex_CombinedStudentsBySchool)
data(ex_CombinedAssessmentResults)

sel_names <- names(ex_CombinedStudentsBySchool)

sel_cdf_names <- names(ex_CombinedAssessmentResults)


map_students <- map_kipp[, c(sel_names)] %>%
  mutate(school = abbrev(SchoolName, exceptions = list(old = c("KAPS", "KAMS"),
                                                       new = c("KACP", "KACP")
  )
  )
  ) %>%
  unique

map_cdf <- map_kipp %>%
  select(-StudentLastName:-Grade) %>%
  as.data.frame


map_kipp <- mapvizieR(map_cdf, map_students)


 map_kipp$cdf<-mutate(map_kipp$cdf, schoolname=ifelse(grepl("Ascend", schoolname),
                                "KIPP Ascend College Prep",
                                schoolname
                                )
                          )

summary(map_kipp)



# Math ####
map_kipp_spring <- filter(map_kipp$cdf,
                        fallwinterspring=="Spring",
                        map_year_academic == 2014)



map_kipp_spring %>% group_by(measurementscale, schoolname
                                      ) %>%
  summarise(N=n())

group_by(map_cps, School) %>% summarise(N=n())


recon_cps_has_kipp_doesnt<-anti_join(map_cps, map_kipp_spring, by=c("StudentID"="studentid")) %>%
  select(StudentFirstname=First.Name, StudentLastname=Last.Name, StudentID, Grade=Student.s.Annualized.Grade, School)

recon_kipp_has_cps_doesnt_math<-filter(map_kipp_spring, measurementscale=="Mathematics") %>% anti_join(map_cps,  by=c("studentid"="StudentID")) %>%
  select(studentid, grade, schoolname)

recon_kipp_has_cps_doesnt_read<-filter(map_kipp_spring, measurementscale=="Reading") %>% anti_join(map_cps,  by=c("studentid"="StudentID")) %>%
  select(studentid, grade, schoolname)


#check rit scores

# Math
math_diff<-filter(map_kipp_spring, measurementscale=="Mathematics") %>%
  inner_join(map_cps, by=c("studentid" = "StudentID")) %>%
  select(studentid,
         Last.Name,
         First.Name,
         testritscore,
         testpercentile,
         Math.RIT.Score.Spring.2015,
         Math.Percentile.Spring.2015) %>%
  mutate(RIT_Diff=testritscore-Math.RIT.Score.Spring.2015,
         Pctl_Diff=testpercentile-Math.Percentile.Spring.2015) %>%
  filter(RIT_Diff!=0 | Pctl_Diff!=0)

# Reading
reading_diff<-filter(map_kipp_spring, measurementscale=="Reading") %>%
  inner_join(map_cps, by=c("studentid" = "StudentID")) %>%
  select(studentid,
         Last.Name,
         First.Name,
         testritscore,
         testpercentile,
         Reading.RIT.Score.Spring.2015,
         Reading.Percentile.Spring.2015) %>%
  mutate(RIT_Diff=testritscore-Reading.RIT.Score.Spring.2015,
         Pctl_Diff=testpercentile-Reading.Percentile.Spring.2015) %>%
  filter(RIT_Diff!=0 | Pctl_Diff!=0)


# Attendence data ####
att_cps<-rbind(Attd.Review.RosterSY14.400044, Attd.Review.RosterSY14.400146,Attd.Review.RosterSY14.400163)

att_cps$School<-mapply(switch, as.character(att_cps$School.ID),
                       MoreArgs = list("400044"="Ascend",
                                       "400146"="KCCP",
                                       "400163"="KBCP")
)

att_cps$StudentID<-att_cps$Student.ID

Attendance$Schools<-mapply(switch, as.character(Attendance$SCHOOLID),
                           MoreArgs = list("400044"="Ascend",
                                           "400146"="KCCP",
                                           "400163"="KBCP")
)

att_kipp<-filter(Attendance, ymd_hms(CALENDARDATE)>=ymd("2013-08-19")) %>%
  group_by(
                   STUDENTID,
                   LASTFIRST,
                   Schools,
                   GRADE_LEVEL
                   ) %>%
  summarise(Membershipe_Days=sum(ENROLLED),
            Total_Days_Present=sum(1-ABSENT)
            )

att_kipp$StudentID<-as.integer(att_kipp$STUDENTID)

enrolled_kipp_has_cps_doesnt<-anti_join(att_kipp, att_cps, by="StudentID")

enrolled_cps_has_kipp_doesnt<-anti_join(att_cps,att_kipp,  by="StudentID")


