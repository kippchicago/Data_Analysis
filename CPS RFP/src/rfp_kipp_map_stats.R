setwd("~/Dropbox (KIPP Chicago Schools)/Data Analysis/CPS RFP/")

require(ProjectTemplate)
load.project()


#Get Fall to Spring % M/E across whole network for Reading and Math
get_network_stats(start_season = "FALL", low_grade=0, high_grade = 12)
get_network_stats(start_season = "FALL", low_grade=0, high_grade = 8)
get_network_stats(start_season = "FALL", low_grade=3, high_grade = 8)



get_network_stats(start_season = "SPRING", low_grade=0, high_grade = 12)
get_network_stats(start_season = "SPRING", low_grade=0, high_grade = 8)
get_network_stats(start_season = "SPRING", low_grade=3, high_grade = 8)


# CPS ###
cps_map %>%
  filter(Grade=="All Grades Combined") %>%
  dplyr::summarize(n_stus=sum(n_tested, na.rm = TRUE),
                   n_met=sum(n_met, na.rm = TRUE),
                   pct_met=round(n_met/n_stus*100),
                   n=n(),
                   n_over_70=sum(over_70, na.rm = TRUE),
                   pct_over_70=round(n_over_70/n*100)
  )
