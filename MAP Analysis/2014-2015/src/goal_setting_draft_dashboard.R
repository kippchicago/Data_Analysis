# analysis for draft dashboard

setwd("~/Dropbox (KIPP Chicago Schools)/Data Analysis/MAP Analysis/2014-2015")

require(ProjectTemplate)
load.project()

map_all<- map_all %>% mutate(MeasurementScale=ifelse(MeasurementScale=="Science - Concepts and Process",
                                                     "General Science",
                                                     MeasurementScale)
                             )
map_mv<-mapvizier(map_all)
map_mv_summ <- summary(map_mv)


glimpse(map_mv_summ)

map_mv_summ_by_grade <- map_mv_summ %>% data.frame %>% 
  dplyr::filter(Subject!="Language Usage", N>10) %>%
  group_by(SY, GrowthSeason, Subject, Grade) %>% 
  summarize(N=sum(N), 
            N_Typical=sum(N_Typical), Pct_Typical=N_Typical/N,
            N_50th = sum(N_50th_Pctl_S2,na.rm=T), Pct_50th = N_50th/N,
            N_75th = sum(N_75th_Pctl_S2, na.rm=T), Pct_75th = N_75th/N
  )




map_mv_summ_region <- map_mv_summ_by_grade %>%
  summarize(N=sum(N), 
            N_Typical=sum(N_Typical), Pct_Typical=N_Typical/N,
            N_50th = sum(N_50th), Pct_50th = N_50th/N,
            N_75th = sum(N_75th), Pct_75th = N_75th/N) %>%
  mutate(Grade=99)# 99=All grades combined


map_summary<-rbind_all(list(map_mv_summ_region, map_mv_summ_by_grade)) %>%
  mutate(Year2=as.integer(gsub("\\d+-", "", SY)),
         CohortYear=Year2 + (12-Grade)) %>%
  arrange(GrowthSeason, Subject, Grade, Year2) %>%
  group_by(GrowthSeason, Subject, Grade) %>%
  mutate(YLag_Typical=lag(Pct_Typical),
         YLag_50=lag(Pct_50th),
         YLag_75=lag(Pct_75th)) %>%
  arrange(GrowthSeason, Subject, CohortYear) %>%
  group_by(GrowthSeason, Subject, CohortYear) %>%
  mutate(CLag_Typical=lag(Pct_Typical),
         CLag_50=lag(Pct_50th),
         CLag_75=lag(Pct_75th))


map_summary %>% filter(Year2==2014) %>% 
  arrange(Grade, Subject) %>% 
  write.csv(x=., file="reports/dashboard_map.csv")


# Get KIPP Network MAP data
map_kipp <- read.csv("../../HSR//data//Historical_MAP_2014 07 21.csv")

map_kipp %>% mutate(Pct_CR = as.numeric(gsub("%", "", met_tiered))/100,
                    Pct_Typical = as.numeric(gsub("%", "", met_typical))/100) %>%
  filter(Start_Season=="SPRING", End_Season=="SPRING", Growth_Academic_Year==2013) %>%
  group_by(Sub_Test_Name) %>%
  summarize(Pct_Typical=sum(Pct_Typical*n_count)/sum(n_count))
