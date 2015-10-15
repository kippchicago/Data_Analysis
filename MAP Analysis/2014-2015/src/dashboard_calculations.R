map_mv_all$cdf %>%
  mutate(cohort_year = map_year_academic + 1 + 12 - grade) %>%
  filter(cohort_year==2020
         ) %>%
  group_by(measurementscale, fallwinterspring, grade, termname, schoolname) %>%
  summarize(N = n(),
            n_50 = sum(testpercentile>=75),
            pct = round(n_50/N*100,1)) %>%
  filter(fallwinterspring == "Spring", grepl("Read", measurementscale)) %>%
  arrange(measurementscale, termname)

# Region
map_mv_sum <- summary(map_mv_all)
map_mv_sum %>%
  filter(growth_window == "Spring to Spring",
         end_grade>=3,
         end_map_year_academic == 2014) %>%
  group_by(measurementscale) %>%
  summarize(n_students = sum(n_students),
            n_typical = sum(n_typical),
            pct_typical = round(n_typical/n_students*100,1))

# By school
map_mv_sum %>%
  filter(growth_window == "Spring to Spring",
         end_grade>=3,
         end_map_year_academic == 2014) %>%
  group_by(measurementscale, end_schoolname) %>%
  summarize(n_students = sum(n_students),
            n_typical = sum(n_typical),
            pct_typical = round(n_typical/n_students*100,1))

# Region 2013-14 and 2012-2013
map_mv_sum %>%
  filter(growth_window == "Spring to Spring",
         end_grade>=3,
         end_map_year_academic %in% c(2013, 2012)) %>%
  group_by(measurementscale, end_map_year_academic) %>%
  summarize(n_students = sum(n_students),
            n_typical = sum(n_typical),
            pct_typical = round(n_typical/n_students*100,1))

