# Fall 2013 to Spring 2014 SQRP analysis 

require(mapvisuals)
require(dplyr)

map.db<-as.data.frame(read.dcf('config//mapdb.dcf'))

mapsrc<-src_mysql(dbname=as.character(map.db$dbname), 
                  host=as.character(map.db$host), 
                  user=as.character(map.db$user), 
                  password=as.character(map.db$password))

# get viewAllAssessments
map.all<-collect(tbl(mapsrc, "viewAllAssessments"))

# get special ed
map.accomodations<-collect(tbl(mapsrc, "tblAccommodationAssignmentSpring14"))

map.sped<-group_by(map.accomodations, StudentID) %>% 
  summarize(N=n()) %>% # get's unique Student ID
  mutate(SPED=1) %>%  # add sped indicator
  select(-N) # drop number of accomodations


# filter to only 2013-14, Fall and Spring, Reading and Math, grade >=2
map.all<-filter(map.all, 
                TermName %in% c("Fall 2013-2014", "Spring 2013-2014"), 
                Grade>=2, 
                MeasurementScale %in% c("Reading", "Mathematics")
                )


# merge test and sped data
map.df<-left_join(map.all, map.sped, by="StudentID") %>% 
  mutate(SPED=ifelse(is.na(SPED),0,1))

map.mv <- mapvizier(map.df)

#subset for sped only
map.mv.sped <- mapvizier(filter(map.df, SPED==1))


#KAP/KAMS Growth ###
filter(map.mv$seasonMatched, SchoolInitials %in% c("KAPS", "KAMS"))  %>% 
  group_by(MeasurementScale, Grade) %>% 
  summarise(N=n(), 
            Fall_RIT=round(mean(TestRITScore),1),
            Spring_RIT=round(mean(TestRITScore.2),1))

#KAP/KAMS Attainment ###
filter(map.mv$mapData, 
       SchoolInitials %in% c("KAPS", "KAMS"),
       Season=="Spring")  %>% 
  group_by(MeasurementScale, Grade) %>% 
  summarise(N=n(), 
            Spring_RIT=round(mean(TestRITScore),1)
            )

#KAP/KAMS Pct ME/ Growth ###
filter(as.data.frame(map.mv$seasonMatched), 
       SchoolInitials %in% c("KAPS", "KAMS"),
       Grade>=3)  %>%
  summarise(N=n(), 
            Pct_Met_Typical=round(sum(MetTypical)/N*100,1))


#KAP/KAMS Growth Diverse Learners###
filter(map.mv.sped$seasonMatched, SchoolInitials %in% c("KAPS", "KAMS"))  %>% 
  group_by(MeasurementScale, Grade) %>% 
  summarise(N=n(), 
            Fall_RIT=round(mean(TestRITScore),1),
            Spring_RIT=round(mean(TestRITScore.2),1))

#KAP/KAMS Attainment Diverse Learners###
filter(map.mv.sped$mapData, 
       SchoolInitials %in% c("KAPS", "KAMS"),
       Season=="Spring")  %>% 
  group_by(MeasurementScale, Grade) %>% 
  summarise(N=n(), 
            Spring_RIT=round(mean(TestRITScore),1)
  )

#KCCP Growth ###
filter(map.mv$seasonMatched, SchoolInitials == "KCCP")  %>% 
  group_by(MeasurementScale, Grade) %>% 
  summarise(N=n(), 
            Fall_RIT=round(mean(TestRITScore),1),
            Spring_RIT=round(mean(TestRITScore.2),1))

#KCCP Attainment ###
filter(map.mv$mapData, 
       SchoolInitials=="KCCP",
       Season=="Spring")  %>% 
  group_by(MeasurementScale, Grade) %>% 
  summarise(N=n(), 
            Spring_RIT=round(mean(TestRITScore),1)
  )

#KCCP Pct ME/ Growth ###
filter(as.data.frame(map.mv$seasonMatched), 
       SchoolInitials=="KCCP",
       Grade>=3)  %>%
  summarise(N=n(), 
            Pct_Met_Typical=round(sum(MetTypical)/N*100,1))


#KCCP Growth Diverse Learners###
filter(map.mv.sped$seasonMatched, SchoolInitials=="KCCP")  %>% 
  group_by(MeasurementScale, Grade) %>% 
  summarise(N=n(), 
            Fall_RIT=round(mean(TestRITScore),1),
            Spring_RIT=round(mean(TestRITScore.2),1))

#KCCP Attainment Diverse Learners###
filter(map.mv.sped$mapData, 
       SchoolInitials=="KCCP",
       Season=="Spring")  %>% 
  group_by(MeasurementScale, Grade) %>% 
  summarise(N=n(), 
            Spring_RIT=round(mean(TestRITScore),1)
  )


#KBCP Growth ###
filter(map.mv$seasonMatched, SchoolInitials == "KBCP")  %>% 
  group_by(MeasurementScale, Grade) %>% 
  summarise(N=n(), 
            Fall_RIT=round(mean(TestRITScore),1),
            Spring_RIT=round(mean(TestRITScore.2),1))

#KBCP Attainment ###
filter(map.mv$mapData, 
       SchoolInitials=="KBCP",
       Season=="Spring")  %>% 
  group_by(MeasurementScale, Grade) %>% 
  summarise(N=n(), 
            Spring_RIT=round(mean(TestRITScore),1)
  )

#KBCP Pct ME/ Growth ###
filter(as.data.frame(map.mv$seasonMatched), 
       SchoolInitials=="KBCP",
       Grade>=3)  %>%
  summarise(N=n(), 
            Pct_Met_Typical=round(sum(MetTypical)/N*100,1))


#KBCP Growth Diverse Learners###
filter(map.mv.sped$seasonMatched, SchoolInitials=="KBCP")  %>% 
  group_by(MeasurementScale, Grade) %>% 
  summarise(N=n(), 
            Fall_RIT=round(mean(TestRITScore),1),
            Spring_RIT=round(mean(TestRITScore.2),1))

#KBCP Attainment Diverse Learners###
filter(map.mv.sped$mapData, 
       SchoolInitials=="KBCP",
       Season=="Spring")  %>% 
  group_by(MeasurementScale, Grade) %>% 
  summarise(N=n(), 
            Spring_RIT=round(mean(TestRITScore),1)
  )
