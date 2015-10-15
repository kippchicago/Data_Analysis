map_mv$cdf %>%
  left_join(map_mv$roster %>%
              select(termname,
                     studentid,
                     studentlastname,
                     studentfirstname,
                     school),
            by=c("termname", "studentid")) %>%
  filter(grade == 7,
         school == "KACP",
         measurementscale == "Reading",
         termname == "Spring 2014-2015") %>%
  mutate(on_grade_level = "below",
         on_grade_level = ifelse(testpercentile >=25,
                                 "on",
                                 on_grade_level),
         on_grade_level = ifelse(testpercentile >= 75,
                                 "above",
                                 on_grade_level),
         duration_hours = round(testdurationminutes/60,1)) %>%
  select(school,
         grade,
         studentid,
         last = studentlastname,
         first =studentfirstname,
         RIT_score = testritscore,
         percentile = testpercentile,
         quartile = testquartile,
         on_grade_level,
         duration_mins = testdurationminutes,
         duration_hours
  ) %>%
  arrange(desc(RIT_score), last, first) %>%
  readr::write_csv("reports/Harrison_7th_KACP_Reading_Scores.csv")

