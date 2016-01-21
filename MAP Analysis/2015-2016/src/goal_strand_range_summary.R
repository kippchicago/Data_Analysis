map_2020_math <- map_viz$cdf %>%
  mutate(cohort = map_year_academic + 1 + 12 - grade) %>%
  filter(cohort == 2020,
         measurementscale == "Mathematics")

m_goal_scores <- map_2020_math %>%
  select(studentid, testid, measurementscale, schoolname, cohort, termname, fallwinterspring, map_year_academic, grade,
         matches("(goal)[0-9]ritscore"))

m_goal_stderr <- map_2020_math %>%
  select(studentid, testid, measurementscale, schoolname, cohort, termname, fallwinterspring, map_year_academic, grade,
         matches("(goal)[0-9]stderr"))

m_goal_names <- map_2020_math %>%
  select(studentid, testid, measurementscale, schoolname, cohort, termname, fallwinterspring, map_year_academic, grade,
         matches("(goal)[0-9](name)"))

m_goal_range <- map_2020_math %>%
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


# summarize those suckers!

goals_summary_by_school <- m_goals %>%
  mutate(x_1_goal_name = ifelse(grepl("Algeb", goal_name),
                                "Algebra/\nOperations and Algebraic Thinking",
                                goal_name),
         x_2_goal_name = ifelse(grepl("Statistics", x_1_goal_name),
                                "Data Analysis, Statistics, and Probability/\nStatistics and Probability",
                                x_1_goal_name),
         goal_name = ifelse(grepl("Number", x_2_goal_name),
                            "Number Sense/\nThe Real and Complex Number Systems",
                            x_2_goal_name)) %>%
  select(-matches("x_")) %>%
  group_by(goal_name, grade, fallwinterspring, map_year_academic, schoolname) %>%
  summarize(mean_score = mean(goal_score),
            mean_stderr = round(sqrt(mean((goal_stderr^2))),1),
            mean_low = mean(goal_low),
            mean_high = mean(goal_high),
            n_students = n()) %>%
  filter(n_students>20) %>%
  ungroup %>%
  mutate(season = factor(fallwinterspring, c("Fall", "Winter", "Spring"), ordered=TRUE))

ggplot(goals_summary_by_school,
       aes(y=mean_score,
           x=season)) +
  geom_linerange(aes(ymin = mean_low,
                     ymax = mean_high,
                     x = season,
                     color=schoolname),
                 position = position_dodge(width = 1)) +

  geom_point(aes(y = mean_score,
                 x = season,
                 color=schoolname),
             position = position_dodge(width = 1)) +
  coord_flip() +
  facet_grid(goal_name ~ grade, switch = "y") +
  theme_light() +
  theme(strip.text.y = element_text(angle = 180))




ggplot(goals_summary_by_school %>%
         filter(season == "Spring"),
       aes(y=mean_score,
           x=season)) +
  geom_linerange(aes(ymin = mean_low,
                     ymax = mean_high,
                     x = season,
                     color=schoolname),
                 position = position_dodge(width = 1)) +

  geom_point(aes(y = mean_score,
                 x = season,
                 color=schoolname),
             position = position_dodge(width = 1)) +
  geom_text(aes(y = mean_low,
                label = round(mean_low),
                color=schoolname),
            position = position_dodge(width = 1),
            hjust=1,
            vjust=1) +
  geom_text(aes(y = mean_high,
                label = round(mean_high),
                color=schoolname),
            position = position_dodge(width = 1),
            hjust=0,
            vjust=1) +
  geom_text(aes(y = mean_score,
                label = round(mean_score),
                color=schoolname),
            position = position_dodge(width = 1),
            hjust=.5,
            vjust=-.7,
            size=4,
            fontface="bold") +
  coord_flip() +
  facet_grid(goal_name ~ grade, switch = "y") +
  theme_light() +
  theme(legend.position = "bottom",
        strip.text.y = element_text(angle = 180)) +
  ylim(c(200, 245))
