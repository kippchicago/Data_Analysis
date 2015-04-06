# RFP KIPP Network MAP Analysis

setwd("~/Dropbox (KIPP Chicago Schools)/Data Analysis/CPS RFP")

map_kipp_pcts<-read.csv("data/2014MAPdistribution_20150317.csv", stringsAsFactors=FALSE)


map_kipp_pcts2<-map_kipp_pcts %>% 
  mutate(at_above_20th=as.numeric(gsub("%", "", at_above_20th))/100,
         at_above_40th=as.numeric(gsub("%", "", at_above_40th))/100,
         at_above_50th=as.numeric(gsub("%", "", at_above_50th))/100)


map_kipp_pcts_by_school<-map_kipp_pcts2 %>%
  filter(Grade %in% c(3:8)) %>%
  group_by(School_ID, School_Display_Name) %>%
  summarize(Pct_50 = weighted.mean(x=at_above_50th, w=n_count),
            Pct_40 = weighted.mean(x=at_above_40th, w=n_count),
            Pct_20 = weighted.mean(x=at_above_20th, w=n_count)) %>%
  ungroup %>%
  arrange(desc(Pct_50))



map_kipp_pcts_by_school %>%
  ungroup %>%
  summarize(N=n(),
            N_BM_50=sum(Pct_50>=.85),
            N_BM_40=sum(Pct_40>=.85),
            N_BM_20=sum(Pct_20>=.85),
            Pct_BM_50=round(N_BM_50/N*100),
            Pct_BM_40=round(N_BM_40/N*100),
            Pct_BM_20=round(N_BM_20/N*100))

