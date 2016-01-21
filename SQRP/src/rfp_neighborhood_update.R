# Infor for RFP update

re

source("src/sqrp_2_year_avg_charts.R")

sqrp %>%
  filter(sy == "SY2015-2016") %>%
  group_by(COMMAREA, rating) %>%
  summarize(Number = n()) %>%
  filter(COMMAREA %in% c("AUSTIN", "WEST GARFIELD PARK", "HUMBOLDT PARK")) %>%
  summarize(one_one_plus = sum(ifelse(grepl("1", rating), Number, 0)), Number = sum(Number),
            pct = round(one_one_plus/Number * 100))
