# more 6th grade analysis for Amy 1/20/2015


ps_6th<-current_ps_roster %>%
  dplyr::filter(Grade_Level == 6) %>%
  dplyr::mutate(studentid = StudentID)

current_cdf <- map_viz$cdf %>%
  dplyr::inner_join(ps_6th,
             by = "studentid")



current_growth <-  map_viz$growth_df %>%
  dplyr::inner_join(ps_6th,
                    by = "studentid")

current_cdf %>%
  filter(termname == "Spring 2014-2015",
         measurementscale == "Mathematics", grade ==5) %>%
  mutate(gt_231 = testritscore >= 231) %>%
  group_by(school_initials) %>%
  summarize(mean_rit = mean(testritscore),
            mean_typgrowth = mean(springtospringprojectedgrowth),
            total_gt_231 = sum(gt_231)) %>%
  mutate(mean_goal = mean_rit + mean_typgrowth)


current_cdf %>%
  filter(termname == "Winter 2014-2015",
         measurementscale == "Mathematics", grade ==5) %>%
  mutate(gt_231 = testritscore >= 231) %>%
  group_by(school_initials) %>%
  summarize(mean_rit = mean(testritscore),
            mean_typgrowth = mean(springtospringprojectedgrowth),
            total_gt_231 = sum(gt_231)) %>%
  mutate(mean_goal = mean_rit + mean_typgrowth)


current_cdf %>%
  filter(termname == "Fall 2015-2016",
         measurementscale == "Mathematics", grade ==6) %>%
  group_by(school_initials) %>%
  summarize(mean_rit = mean(testritscore)) %>%
  mutate(mean_goal = mean_rit + mean_typgrowth)


current_cdf %>%
  filter(termname == "Winter 2015-2016",
         measurementscale == "Mathematics", grade ==6) %>%
  group_by(school_initials) %>%
  summarize(mean_rit = mean(testritscore))


# Growth goals ####
current_growth %>%
  filter(measurementscale == "Mathematics", start_grade ==6,
         start_map_year_academic == 2015,
         growth_window == "Fall to Spring") %>%
  group_by(start_schoolname) %>%
  summarize(mean_rit = mean(reported_growth))
