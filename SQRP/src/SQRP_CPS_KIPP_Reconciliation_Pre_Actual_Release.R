# SQRP CSP vs KIPP Chicago Reconciliation
# NWEA MAP Munging Script

# rbind all NWEA MAP tables from CPS into one table

map_cps<-rbind(SY13.14.NWEA.Roster.400044, 
               SY13.14.NWEA.Roster.400146,  
               SY13.14.NWEA.Roster.400163
               ) 
  


#map_cps_read <- select(StudentID, RD_SQRP_RIT_PRE_GRWTH, RD_SQRP_RIT_SPR_CY_ATTAIN, MeasurementScale="Reading")


map_cps$School<-mapply(switch, as.character(map_cps$STUDENT_SCHOOL_ID_ANNUAL),
       MoreArgs = list("400044"="Ascend", 
                       "400146"="KCCP", 
                       "400163"="KBCP")
)

map_cps$StudentID <- map_cps$STUDENT_ID
map_cps_read_pre <- select(map_cps, 
                           StudentID, 
                           CPSTestRIT_Fall = RD_SQRP_RIT_PRE_GRWTH,
                           CPSTestRIT_Spring = RD_SQRP_RIT_POST_GRWTH) %>%
  mutate(MeasurementScale="Reading",
         StudentID=as.integer(as.character(StudentID)),
         CPSTestRIT_Fall=as.integer(as.character(CPSTestRIT_Fall)),
         CPSTestRIT_Spring=as.integer(as.character(CPSTestRIT_Spring)))

map_cps_math_pre <- select(map_cps, 
                           StudentID, 
                           CPSTestRIT_Fall = MT_SQRP_RIT_PRE_GRWTH,
                           CPSTestRIT_Spring = MT_SQRP_RIT_POST_GRWTH) %>%
  mutate(MeasurementScale="Mathematics", 
         StudentID=as.integer(as.character(StudentID)),
         CPSTestRIT_Fall=as.integer(as.character(CPSTestRIT_Fall)),
         CPSTestRIT_Spring=as.integer(as.character(CPSTestRIT_Spring))
  )


map_cps2 <- rbind(map_cps_math_pre, map_cps_read_pre)
      
map_kipp<-filter(map_all, 
                 TermName %in% c("Fall 2013-2014", "Spring 2013-2014"), 
                 MeasurementScale %in% c("Mathematics", "Reading"),
                 Grade>=3
                 ) %>%
  mapvizier


map_kipp_2<-map_kipp$seasonMatched %>% as.data.frame %>%
  select(StudentID,
         SchoolName,
         Grade,
         StudentLastname, 
         StudentFirstname,
         MeasurementScale,
         RIT_Fall = TestRITScore,
         RIT_Spring = TestRITScore.2
         ) %>%
  mutate(RIT_Fall_Spring_Equated=round(cps_equate(RIT_Fall, 
                                            MeasurementScale,
                                            Grade)))



kipp_cps<-inner_join(map_kipp_2, map_cps2, by=c("StudentID", "MeasurementScale"))

# get actual Spring 12-13
map_s13 <- map_all %>% 
  filter(TermName =="Spring 2012-2013") %>% 
  select(StudentID,  MeasurementScale, RIT_Actual_Spring13=TestRITScore)

kipp_cps_2 <-inner_join(kipp_cps, map_s13, by=c("StudentID", "MeasurementScale"))


# Check to see if the model is any good ####
kipp_cps_2 %>% 
  mutate(Diff=RIT_Fall_Spring_Equated - RIT_Actual_Spring13) %>%
  group_by(MeasurementScale, Grade, SchoolName) %>%
  dplyr::summarize(Mean_Diff=mean(Diff, na.rm=T), 
            N=n(), 
            N_Under_Predict = sum(Diff<0, na.rm=T), 
            Pct_Under_Predict=N_Under_Predict/N*100) %>%
  filter(N>=10) %>%
  arrange(Pct_Under_Predict)

# Results: it is not any good. mean residuals are not near
# (except 3 Math.  Pct_Under_predicted not near 50% in 
# any grade subject)

kipp_cps %>%
  #mutate(CPSTestRIT=ifelse(is.na(CPSTestRIT), RIT_Spring, CPSTestRIT)) %>%
  group_by(MeasurementScale, Grade, SchoolName) %>%
  dplyr::summarize(n(),
            Mean_Start=mean(RIT_Fall_Spring_Equated),
            Mean_End = mean(CPSTestRIT_Spring,na.rm=T)) %>% ungroup %>%
  arrange(SchoolName, MeasurementScale,Grade)


# Get % M/E ####
map_kipp_matched<-map_kipp$seasonMatched %>% 
  as.data.frame %>% 
  filter(Grade==5) %>%
  mutate(TestRITScore_imputed=round(cps_equate(TestRITScore, 
                                                  MeasurementScale,
                                                  Grade)),
         TypicalGrowth=R22,
         TypicalTarget=TestRITScore_imputed + TypicalGrowth,
         MetTypical=TestRITScore.2>=TypicalTarget)

map_kipp_matched %>% 
  group_by(SchoolInitials, Grade, MeasurementScale) %>%
  dplyr::summarize(N=n(), 
                   N_Met=sum(MetTypical), 
                   Pct_Met=round(N_Met/N*100),
                   Mean_Spring_1=round(mean(TestRITScore_imputed)),
                   Mean_spring_2=round(mean(TestRITScore.2))) %>%
  arrange(MeasurementScale, SchoolInitials)
  
