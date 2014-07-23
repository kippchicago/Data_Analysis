# SQRP CSP vs KIPP Chicago Reconciliation
# NWEA MAP Munging Script

# rbind all NWEA MAP tables from CPS into one table

map_cps<-rbind(NWEA.Review.RosterSY14.400044, 
               NWEA.Review.RosterSY14.400146,  
               NWEA.Review.RosterSY14.400163
               )

map_cps$School<-mapply(switch, as.character(map_cps$Student.s.Annualized.School.ID),
       MoreArgs = list("400044"="Ascend", 
                       "400146"="KCCP", 
                       "400163"="KBCP")
)

map_cps$StudentID <- map_cps$Student.ID
      
map_kipp<-filter(map_all, 
                 TermName %in% c("Fall 2013-2014", "Spring 2013-2014"), 
                 MeasurementScale %in% c("Mathematics", "Reading"),
                 Grade>=2
                 )




map_kipp<-mapvizier(map_kipp)


map_kipp$mapData<-mutate(map_kipp$mapData, School=ifelse(SchoolInitials %in% c("KAPS", "KAMS"),
                               "Ascend",
                               SchoolInitials
                               )
                         )

summary(map_kipp)



# Math ####
map_kipp_spring <- filter(map_kipp$mapData, 
                        Season=="Spring")
                        
                        

map_kipp_spring %>% group_by(MeasurementScale,
                                      School
                                      ) %>% 
  summarise(N=n())

group_by(map_cps, School) %>% summarise(N=n())


recon_cps_has_kipp_doesnt<-anti_join(map_cps, map_kipp_spring, by="StudentID") %>%
  select(StudentFirstname=First.Name, StudentLastname=Last.Name, StudentID, Grade=Student.s.Annualized.Grade, School)
recon_kipp_has_cps_doesnt_math<-filter(map_kipp_spring, MeasurementScale=="Mathematics") %>% anti_join(map_cps,  by="StudentID") %>%
  select(StudentFirstname, StudentLastname, StudentID, Grade, School)
recon_kipp_has_cps_doesnt_read<-filter(map_kipp_spring, MeasurementScale=="Reading") %>% anti_join(map_cps,  by="StudentID") %>%
  select(StudentFirstname, StudentLastname, StudentID, Grade, School)


#check rit scores

# Math
filter(map_kipp_spring, MeasurementScale=="Mathematics") %>% 
  inner_join(map_cps, by="StudentID") %>%
  select(StudentID, 
         Last.Name, 
         First.Name, 
         StudentLastname, 
         StudentFirstname,
         TestRITScore,
         TestPercentile,
         Math.RIT.Score.Spring.2014,
         Math.Percentile.Spring.2014) %>%
  mutate(RIT_Diff=TestRITScore-Math.RIT.Score.Spring.2014,
         Pctl_Diff=TestPercentile-Math.Percentile.Spring.2014) %>%
  filter(RIT_Diff!=0 | Pctl_Diff!=0)

# Reading
x<-filter(map_kipp_spring, MeasurementScale=="Reading") %>% 
  inner_join(map_cps, by="StudentID") %>%
  select(StudentID, 
         Last.Name, 
         First.Name, 
         StudentLastname, 
         StudentFirstname,
         TestRITScore,
         TestPercentile,
         Reading.RIT.Score.Spring.2014,
         Reading.Percentile.Spring.2014) %>%
  mutate(RIT_Diff=TestRITScore-Reading.RIT.Score.Spring.2014,
         Pctl_Diff=TestPercentile-Reading.Percentile.Spring.2014) %>%
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

#quick look at billy's kids in Spring 13 and spring 14

map<-mapvizier(map_all)
billy<-filter(map$mapData, 
              SchoolInitials=="KCCP", 
              MeasurementScale=="Reading", 
              CohortYear==2020, 
              Season=="Spring")


billy_mean <- group_by(billy, Grade) %>% summarise(TestRITScore=mean(TestRITScore), Min=mean(TestDurationInMinutes))

ggplot(data=billy, 
       aes(y=TestRITScore, x=Grade)) + 
  geom_boxplot(aes(group=Grade), notch=T) + 
  geom_vline(data=billy_mean,
             aes(xintercept=TestRITScore, 
                 color=factor(Grade)))