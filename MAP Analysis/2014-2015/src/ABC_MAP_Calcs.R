# a better chicago growth calculation

require(dplyr)

load("data/map_mv.RDA")
load("data/map_mv_all.RDA")

map_mv_all$growth_df %>% mutate(end_school

not_tested_at_kipp <- map_mv_all$cdf %>%
  filter(tested_at_kipp == "FALSE") %>%
  select(studentid) %>%
  arrange(studentid) %>%
  unique


# get spring 14 to to spring 15, but drop those not tested at KIPP
map_s2s <- map_mv_all$growth_df %>%
  filter(end_grade >= 3,
         growth_window == "Spring to Spring",
         end_map_year_academic == 2014,
         complete_obsv == TRUE) %>%
  anti_join(not_tested_at_kipp, by="studentid")

spring_ids <- map_s2s %>% select(studentid) %>% unique

map_f2s <- map_mv_all$growth_df %>%
  filter(end_grade >= 3,
         growth_window == "Fall to Spring",
         end_map_year_academic == 2014,
         complete_obsv == TRUE) %>%
  anti_join(spring_ids, by="studentid")

map_combined <- rbind_list(map_s2s, map_f2s)


# Region
map_combined %>%
 # filter(!grepl("Science", measurementscale)) %>%
  group_by(measurementscale) %>%
  summarize(N = n(),
            n_met = sum(met_typical_growth),
            pct_met = n_met/N*100)

# By School
map_combined %>%
  # filter(!grepl("Science", measurementscale)) %>%
  group_by(measurementscale, end_schoolname) %>%
  summarize(N = n(),
            n_met = sum(met_typical_growth),
            pct_met = n_met/N*100)





# as above for 13-14
map_s2s_1314 <- map_mv_all$growth_df %>%
  filter(end_grade >= 3,
         growth_window == "Spring to Spring",
         end_map_year_academic == 2013,
         complete_obsv == TRUE) %>%
  anti_join(not_tested_at_kipp, by="studentid")

spring_ids_1314 <- map_s2s %>% select(studentid) %>% unique

map_f2s_1314 <- map_mv$growth_df %>%
  filter(end_grade >= 3,
         growth_window == "Fall to Spring",
         end_map_year_academic == 2013,
         complete_obsv == TRUE) %>%
  anti_join(spring_ids, by="studentid")

map_combined_1314 <- rbind_list(map_s2s_1314, map_f2s_1314)

map_combined_1314 %>%
  filter(!grepl("Science", measurementscale)) %>%
  summarize(N = n(),
            n_met = sum(met_typical_growth),
            pct_met = n_met/N*100)


