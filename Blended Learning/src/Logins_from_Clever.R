library(tidyverse)
library(lubridate)
library(kippcolors)


ggplot2::theme_set(theme_kipp_light())

x <- read_csv("~/Downloads/KippChicagoPortalLogins.csv")

schools <- 
  tribble(
    ~school_id,                 ~school,
    "5092c74f40c5ecf521143b2d", "KAC",
    "5092c74f40c5ecf521143b2a", "KAMS",
    "5092c74f40c5ecf521143b2c", "KAP",
    "520e72a37a153bb34f031d4f", "KBCP",
    "5b6e1623ee26de0e5b6d7208", "KBP",
    "57b382244a7af0e37f000010", "KOA",
    "57b382244a7af0e37f000009", "KOP"
  )

x %>% glimpse()


x %>% 
  janitor::clean_names() %>%
  filter(user_type == 'student') %>%
  mutate(date = lubridate::date(time)) %>%
  filter(date <= ymd("2018-10-15")) %>%
  inner_join(schools, by = "school_id") %>%
  group_by(date, school, user_type) %>%
  tally() %>%
ggplot(aes(x = date, y = n)) +
  geom_col(aes(fill = date < ymd("2018-09-04"))) +
  geom_vline(aes(xintercept = ymd("2018-09-04"))) +
  facet_grid(school ~ .) +
  scale_fill_kipp(palette = "kipp_greengray") +
  labs(fill = "Date before\nSept 5, 2018",
       x = "Login date",
       y = "Number of Students",
       title = "Very few students logged into EdTech before Labor Day",
       subtitle = "Sept 15th and Oct 1st witness login spikes across the region",
       caption = "Source: Clever")
