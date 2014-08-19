map_mv$seasonMatched %>% data.frame %>%
  filter(GrowthSeason=="Fall - Spring", 
         Year2.2==2014, 
         MeasurementScale %in% c("Reading", "Mathematics")
         ) %>% 
  mutate(MetTypicalLT0=((TestRITScore.2-TestRITScore)-TypicalGrowth<=0) &
           MetTypical==TRUE,
         MetTypicalLT1=((TestRITScore.2-TestRITScore)-TypicalGrowth<=1) &
            MetTypical==TRUE,
         MetTypicalLT2=((TestRITScore.2-TestRITScore)-TypicalGrowth<=2) &
           MetTypical==TRUE,
         MetTypicalLT3=((TestRITScore.2-TestRITScore)-TypicalGrowth<=3) &
           MetTypical==TRUE) %>% 
  group_by(MeasurementScale, 
           SchoolInitials, 
           Grade
           ) %>%
  dplyr::summarize(N=n(), 
                   N_Met=sum(MetTypical), 
                   Pct_Met=N_Met/N,
                   N_Met_LTE1=sum(MetTypicalLT1),
                   N_Met_GT1=N_Met-N_Met_LTE1,
                   Pct_Met_LT1=N_Met_LTE1/N,
                   Pct_Met_GT1=N_Met_GT1/N,
                   N_Met_LTE0=sum(MetTypicalLT0),
                   N_Met_GT0=N_Met-N_Met_LTE0,
                   Pct_Met_LT0=N_Met_LTE0/N,
                   Pct_Met_GT0=N_Met_GT0/N,
                   N_Met_LTE2=sum(MetTypicalLT2),
                   N_Met_GT2=N_Met-N_Met_LTE2,
                   Pct_Met_LT2=N_Met_LTE2/N,
                   Pct_Met_GT2=N_Met_GT2/N,
                   N_Met_LTE3=sum(MetTypicalLT3),
                   N_Met_GT3=N_Met-N_Met_LTE3,
                   Pct_Met_LT3=N_Met_LTE3/N,
                   Pct_Met_GT3=N_Met_GT3/N
                   ) %>%
  select(Subject=MeasurementScale,
         School=SchoolInitials,
         Grade,
         Pct_Met,
         Pct_Met_GT0,
         Pct_Met_GT1,
         Pct_Met_GT2,
         Pct_Met_GT3)



map_mv$seasonMatched %>%  
  filter(MeasurementScale %in% c("Reading", "Mathematics"),
         GrowthSeason=="Fall - Spring",
         Year2.2==2014,
         Grade.2>=3) %>% data.frame %>%
  mutate(SchoolInitials=ifelse(SchoolInitials=="KAPS", "KAMS", SchoolInitials)) %>%
  group_by(MeasurementScale, SchoolInitials, Grade.2) %>%
  summarize(N=n(), 
            Mean_RIT1=mean(TestRITScore),
            Mean_RIT2=mean(TestRITScore.2))