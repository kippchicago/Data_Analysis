#### Growth percentages from last year
#require(ProjectTemplate)
#setwd("~/Dropbox/Consulting/KIPP Ascend/Data Analysis/SQRP")
#load.project()
require(mapvisuals)
source("data/map_all_silo.R")

info(logger, "Get Data Spring 14 - Winter 15 data")


map_mv<-mapvizier(map_all_silo %>% 
                    filter(TermName %in% c("Spring 2013-2014", 
                                           "Winter 2014-2015"
                                           ),
                           MeasurementScale %in% c("Reading",
                                                   "Mathematics"
                                                   )
                           )
                  )


map_mv_2<-mapvizier(map_all_silo %>% 
                      filter(TermName %in% c("Spring 2012-2013", 
                                             "Winter 2013-2014"
                      ),
                      MeasurementScale %in% c("Reading",
                                              "Mathematics"
                      )
                      )
)

map_summ<-summary(map_mv)

ieps <- read.csv("excel files/Winter 2015 Analysis/ieps.csv")

info(logger, "Projecting Future Growth")
map_sqpr_projections <- map_mv$seasonMatched %>%
  mutate(rit_growth=TestRITScore.2-TestRITScore,
         adj_rit_growth=ifelse(rit_growth<0, 0, rit_growth),
         end_rit_1_5 = TestRITScore +  round(1.5*adj_rit_growth),
         end_rit_2 = TestRITScore +  round(2*adj_rit_growth),
         end_rit_typical_ws = TestRITScore.2 + round(T12),
         end_rit_cr_ws = TestRITScore.2 + 
           round(tiered_growth(TestQuartile.2,grade = Grade.2)*T12),
         end_rit_typical_ss = TestRITScore + round(T22),
         end_rit_cr_ss = TestRITScore + 
           round(tiered_growth(TestQuartile,grade = Grade)*T22),
         met_1.5=end_rit_1_5>=TestRITScore+R22,
         met_2=end_rit_2>=TestRITScore+R22,
         met_typical_ws=end_rit_typical_ws>=TestRITScore+R22,
         met_cr_ws=end_rit_cr_ws>=TestRITScore+R22,
         met_typical_ss=end_rit_typical_ss>=TestRITScore+R22,
         met_cr_ss=end_rit_cr_ss>=TestRITScore+R22
         )


map_sqpr_projections_2 <- map_mv_2$seasonMatched %>%
  mutate(rit_growth=TestRITScore.2-TestRITScore,
         adj_rit_growth=ifelse(rit_growth<0, 0, rit_growth),
         end_rit_1_5 = TestRITScore +  round(1.5*adj_rit_growth),
         end_rit_2 = TestRITScore +  round(2*adj_rit_growth),
         end_rit_typical_ws = TestRITScore.2 + round(T12),
         end_rit_cr_ws = TestRITScore.2 + 
           round(tiered_growth(TestQuartile.2,grade = Grade.2)*T12),
         end_rit_typical_ss = TestRITScore + round(T22),
         end_rit_cr_ss = TestRITScore + 
           round(tiered_growth(TestQuartile,grade = Grade)*T22),
         met_1.5=end_rit_1_5>=TestRITScore+R22,
         met_2=end_rit_2>=TestRITScore+R22,
         met_typical_ws=end_rit_typical_ws>=TestRITScore+R22,
         met_cr_ws=end_rit_cr_ws>=TestRITScore+R22,
         met_typical_ss=end_rit_typical_ss>=TestRITScore+R22,
         met_cr_ss=end_rit_cr_ss>=TestRITScore+R22)


info(logger, "calculate avg rit scores")





info(logger, "calculate avg rit scores")






map_sqpr_projections_avgs<-map_sqpr_projections %>%
  as.data.frame %>%
  group_by(MeasurementScale, SchoolInitials, Grade.2) %>%
  dplyr::summarize(N=n(),
            avg_rit_beg=round(mean(TestRITScore, na.rm=TRUE),1),
            avg_rit_end_1=round(mean(TestRITScore.2, na.rm=TRUE),1),
            avg_rit_end_1.5=round(mean(end_rit_1_5, na.rm=TRUE),1),
            avg_rit_end_2=round(mean(end_rit_2, na.rm=TRUE),1),
            avg_rit_end_typical_ws=round(mean(end_rit_typical_ws, na.rm=TRUE),1),
            avg_rit_end_cr_ws=round(mean(end_rit_cr_ws, na.rm=TRUE),1),
            avg_rit_end_typical_ss=round(mean(end_rit_typical_ss, na.rm=TRUE),1),
            avg_rit_end_cr_ss=round(mean(end_rit_cr_ss, na.rm=TRUE),1),
            pct_1.5=round(sum(met_1.5)/N*100,1),
            pct_1=round(sum(met_1.5)/N*100,1),
            pct_typical_ws=round(sum(met_typical_ws)/N*100,1),
            pct_cr_ws=round(sum(met_cr_ws)/N*100,1),
            pct_typical_ss=round(sum(met_typical_ss)/N*100,1),
            pct_cr_ss=round(sum(met_cr_ss)/N*100,1),
            N_1.5=round(sum(met_1.5)),
            N_2=round(sum(met_2)),
            N_typical_ws=round(sum(met_typical_ws)),
            N_cr_ws=round(sum(met_cr_ws)),
            N_typical_ss=round(sum(met_typical_ss)),
            N_cr_ss=round(sum(met_cr_ss))
  )


map_sqpr_projections_avgs_ieps<-map_sqpr_projections %>% 
  as.data.frame %>%
  inner_join(ieps %>% select(Student_Number), 
             by=c("StudentID"="Student_Number")) %>%
  group_by(MeasurementScale, SchoolInitials, Grade.2) %>%
  dplyr::summarize(N=n(),
                   avg_rit_beg=round(mean(TestRITScore, na.rm=TRUE),1),
                   avg_rit_end_1=round(mean(TestRITScore.2, na.rm=TRUE),1),
                   avg_rit_end_1.5=round(mean(end_rit_1_5, na.rm=TRUE),1),
                   avg_rit_end_2=round(mean(end_rit_2, na.rm=TRUE),1),
                   avg_rit_end_typical_ws=round(mean(end_rit_typical_ws, na.rm=TRUE),1),
                   avg_rit_end_cr_ws=round(mean(end_rit_cr_ws, na.rm=TRUE),1),
                   avg_rit_end_typical_ss=round(mean(end_rit_typical_ss, na.rm=TRUE),1),
                   avg_rit_end_cr_ss=round(mean(end_rit_cr_ss, na.rm=TRUE),1),
                   pct_1.5=round(sum(met_1.5)/N*100,1),
                   pct_1=round(sum(met_1.5)/N*100,1),
                   pct_typical_ws=round(sum(met_typical_ws)/N*100,1),
                   pct_cr_ws=round(sum(met_cr_ws)/N*100,1),
                   pct_typical_ss=round(sum(met_typical_ss)/N*100,1),
                   pct_cr_ss=round(sum(met_cr_ss)/N*100,1),
                   N_1.5=round(sum(met_1.5)),
                   N_2=round(sum(met_2)),
                   N_typical_ws=round(sum(met_typical_ws)),
                   N_cr_ws=round(sum(met_cr_ws)),
                   N_typical_ss=round(sum(met_typical_ss)),
                   N_cr_ss=round(sum(met_cr_ss))
  )

map_sqpr_projections_pct_me<-map_sqpr_projections_avgs %>%  
  filter(Grade.2>=3) %>%
  group_by(SchoolInitials) %>%
  dplyr::summarize(N=sum(N),
            pct_1.5=round(sum(N_1.5)/N*100,1),
            pct_2=round(sum(N_2)/N*100,1),
            pct_typical_ws=round(sum(N_typical_ws)/N*100,1),
            pct_cr_ws=round(sum(N_cr_ws)/N*100,1),
            pct_typical_ss=round(sum(N_typical_ss)/N*100,1),
            pct_cr_ss=round(sum(N_cr_ss)/N*100,1))


# KACP  ####
map_sqpr_projections_avgs %>% 
  filter(Grade.2>=2, SchoolInitials=="KACP") %>%
  select(MeasurementScale,
         SchoolInitials, 
         Grade.2,
         avg_rit_end_typical_ss:pct_cr_ss
         ) %>%
arrange(desc(MeasurementScale), Grade.2)




# KACP IEP ####
map_sqpr_projections_avgs_ieps %>% 
  filter(Grade.2>=2, SchoolInitials=="KACP") %>%
  select(MeasurementScale,
         SchoolInitials, 
         Grade.2,
         N,
         avg_rit_beg, 
         avg_rit_end_1.5,
         avg_rit_end_2,
         avg_rit_end_typical_ws,
         avg_rit_end_cr_ws,
         avg_rit_end_typical_ss,
         avg_rit_end_cr_ss) %>%
  arrange(desc(MeasurementScale), Grade.2)


# KCCP  ####
map_sqpr_projections_avgs %>% 
  filter(Grade.2>=2, SchoolInitials=="KCCP") %>%
  select(MeasurementScale,
         SchoolInitials, 
         Grade.2,
         N:pct_cr_ws
  ) %>%
  arrange(desc(MeasurementScale), Grade.2)




# KCCP IEP ####
map_sqpr_projections_avgs_ieps %>% 
  filter(Grade.2>=2, SchoolInitials=="KCCP") %>%
  select(MeasurementScale,
         SchoolInitials, 
         Grade.2,
         N,
         avg_rit_beg, 
         avg_rit_end_1.5,
         avg_rit_end_2,
         avg_rit_end_typical_ws,
         avg_rit_end_cr_ws) %>%
  arrange(desc(MeasurementScale), Grade.2)

# KBCP  ####
map_sqpr_projections_avgs %>% 
  filter(Grade.2>=2, SchoolInitials=="KBCP") %>%
  select(MeasurementScale,
         SchoolInitials, 
         Grade.2,
         N:pct_cr_ws
  ) %>%
  arrange(desc(MeasurementScale), Grade.2)




# KBCP IEP ####
map_sqpr_projections_avgs_ieps %>% 
  filter(Grade.2>=2, SchoolInitials=="KBCP") %>%
  select(MeasurementScale,
         SchoolInitials, 
         Grade.2,
         N,
         avg_rit_beg, 
         avg_rit_end_1.5,
         avg_rit_end_2,
         avg_rit_end_typical_ws,
         avg_rit_end_cr_ws) %>%
  arrange(desc(MeasurementScale), Grade.2)


# Pct Students with negative growth
map_sqpr_projections %>% 
  as.data.frame %>%
  group_by(SchoolInitials, MeasurementScale) %>%
  dplyr::summarize(N=n(),
                   N_neg_growth=sum(rit_growth<0),
                   Pct_neg_growth=round(N_neg_growth/N*100,1))


map_sqpr_projections_2 %>% 
  as.data.frame %>%
  group_by(SchoolInitials, MeasurementScale) %>%
  dplyr::summarize(N=n(),
                   N_neg_growth=sum(rit_growth<0),
                   Pct_neg_growth=round(N_neg_growth/N*100,1))

  