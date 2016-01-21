

source("data/map_all_silo.R")
source("data/current_ps_roster.R")
source("munge/01-A.R")



map_w16 <- map_viz$cdf %>%
  mutate(cohort = map_year_academic + 1 + 12 - grade) %>%
  filter(termname == "Winter 2015-2016")

m_goal_scores <- map_w16 %>%
  select(studentid, testid, measurementscale, schoolname, cohort, termname, fallwinterspring, map_year_academic, grade,
         matches("(goal)[0-9]ritscore"))

m_goal_stderr <- map_w16 %>%
  select(studentid, testid, measurementscale, schoolname, cohort, termname, fallwinterspring, map_year_academic, grade,
         matches("(goal)[0-9]stderr"))

m_goal_names <- map_w16 %>%
  select(studentid, testid, measurementscale, schoolname, cohort, termname, fallwinterspring, map_year_academic, grade,
         matches("(goal)[0-9](name)"))

m_goal_range <- map_w16 %>%
  select(studentid, testid, measurementscale, schoolname, cohort, termname, fallwinterspring, map_year_academic, grade,
         matches("(goal)[0-9](range)"))

# time to gather

m_goal_names_long <- m_goal_names %>%
  gather(key = variable, value = goal_name, goal1name:goal8name)

m_goal_scores_long <- m_goal_scores %>%
  gather(key = variable, value = goal_score, goal1ritscore:goal8ritscore)

m_goal_stderr_long <- m_goal_stderr %>%
  gather(key = variable, value = goal_stderr, goal1stderr:goal8stderr)

m_goal_range_long <- m_goal_range %>%
  gather(key = variable, value = goal_range, goal1range:goal8range)


m_goals <- m_goal_names_long %>%
  mutate(goal_score = m_goal_scores_long$goal_score,
         goal_stderr = m_goal_stderr_long$goal_stderr,
         goal_low = round(goal_score - goal_stderr),
         goal_high = round(goal_score + goal_stderr),
         goal_range = m_goal_range_long$goal_range
  ) %>%
  select(-variable) %>%
  filter(!is.na(goal_name))


mean_strand_scores<-m_goals %>%
  group_by(measurementscale, grade, schoolname, goal_name) %>%
  summarize(n_tests = n(),
            avg_score = round(mean(goal_score),1)
  )

mean_rit_scores <- map_w16 %>%
  group_by(measurementscale, grade, schoolname) %>%
  summarise(n_tests = n(),
            avg_score = round(mean(testritscore),1)) %>%
  mutate(goal_name = "1. Overall RIT Score")

mean_scores <- bind_rows(mean_strand_scores, mean_rit_scores) %>%
  rename(score_type = goal_name)

# averages for KAMS ####
mean_scores_wide <- mean_scores %>%
  filter(grepl("Ascend Middle", schoolname)) %>%
  split(list(.$measurementscale, .$schoolname)) %>%
  map(~spread(., score_type, avg_score))


mean_scores_wide %>%
  map(~ write_csv(., path=sprintf("%s_%s_mean_scores.csv",
                              abbreviate(unique(.$schoolname)),
                              unique(.$measurementscale)
                              )
                 )
      )


# for all schools #####
mean_scores_wide_6_plus <- mean_scores %>%
  filter(grade >= 6)   %>%
  split(list(.$measurementscale)) %>%
  map(~spread(., score_type, avg_score))

mean_scores_wide_2_5 <- mean_scores %>%
  filter(grade %in% c(2:5))   %>%
  split(list(.$measurementscale)) %>%
  map(~spread(., score_type, avg_score))


mean_scores_wide_mpg <- mean_scores %>%
  filter(grade < 2)   %>%
  split(list(.$measurementscale)) %>%
  map(~spread(., score_type, avg_score))


mean_scores_wide_6_plus %>%
  map(~ write_csv(., path=sprintf("reports/%s_mean_scores_6_plus.csv",
                                  unique(.$measurementscale)
  )
  )
  )

mean_scores_wide_2_5 %>%
  map(~ write_csv(., path=sprintf("reports/%s_mean_scores_2_5.csv",
                                  unique(.$measurementscale)
  )
  )
  )

mean_scores_wide_mpg %>%
  map(~ write_csv(., path=sprintf("reports/%s_mean_scores_mpg.csv",
                                  unique(.$measurementscale)
  )
  )
  )

