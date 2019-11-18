library(dplyr)
library(tibble)
library(ggplot2)

sped <- tribble(
  ~grade,  ~subject,  ~assessment, ~category,     ~pct,
       8,  "Math", "NWEA MAP",  "< 25th",      24.6,
       8,  "Math", "NWEA MAP",  "25th - 49th", 25.4,
       8,  "Math", "NWEA MAP",  "≥ 50th",      50,
       7,  "Math", "NWEA MAP",  "< 25th",      33,
       7,  "Math", "NWEA MAP",  "25th - 49th", 28,
       7,  "Math", "NWEA MAP",  "≥ 50th",      39,
       6,  "Math", "NWEA MAP",  "< 25th",      35,
       6,  "Math", "NWEA MAP",  "25th - 49th", 28,
       6,  "Math", "NWEA MAP",  "≥ 50th",      37,
       5,  "Math", "NWEA MAP",  "< 25th",      39.8,
       5,  "Math", "NWEA MAP",  "25th - 49th", 40.4,
       5,  "Math", "NWEA MAP",  "≥ 50th",      19.8,
       4,  "Math", "NWEA MAP",  "< 25th",      39,
       4,  "Math", "NWEA MAP",  "25th - 49th", 39,
       4,  "Math", "NWEA MAP",  "≥ 50th",      21.9,
       3,  "Math", "NWEA MAP",  "< 25th",      54.8,
       3,  "Math", "NWEA MAP",  "25th - 49th", 28.8,
       3,  "Math", "NWEA MAP",  "≥ 50th",      16.3,
       8,  "ELA",  "F&P",       "At Grade Level",    24.9,
       8,  "ELA",  "F&P",       "Below Grade Level", 75.1,
       7,  "ELA",  "F&P",       "At Grade Level",    21.9,
       7,  "ELA",  "F&P",       "Below Grade Level", 78.1,
       6,  "ELA",  "F&P",       "At Grade Level",    18.6,
       6,  "ELA",  "F&P",       "Below Grade Level", 81.4
)

sped %>%
  filter(subject == "Math") %>%
  mutate(category = forcats::fct_inorder(category, ordered =TRUE)) %>%
  ggplot(aes(x = as.factor(grade), y = pct, group = category)) +
  geom_col(aes(fill = category), position = "fill") +
  geom_text(aes(label = pct, color = category == "< 25th"), show.legend = FALSE,
            position = position_fill(vjust= .5)) +
  viridis::scale_fill_viridis("Percentile", discrete = TRUE, direction = -1) +
  scale_color_manual(values = c("white", "black"))+
  theme_linedraw() +
  labs(x ="Grade",
       y = "Percent of student in category",
       title = "In math the # of students at/above grade level is increasing,\nbut at best only 50% of our students are at level to access E/W.",
       subtitle = "Students categorized by national percentile rank on MAP")


sped %>%
  filter(subject == "ELA") %>%
  mutate(category = forcats::fct_inorder(category, ordered =TRUE)) %>%
  ggplot(aes(x = as.factor(grade), y = pct, group = category)) +
  geom_col(aes(fill = rev(category)), position = "fill") +
  geom_text(aes(label = pct, color = category == "At Grade Level"), show.legend = FALSE,
            position = position_fill(vjust= .5)) +
  viridis::scale_fill_viridis("Percentile", discrete = TRUE, direction = 1) +
  scale_color_manual(values = c("white", "black"))+
  theme_linedraw() +
  labs(x ="Grade",
       y = "Percent of student in category",
       title = "In reading the # of students at/above grade level is constant\nand impediment ot  to accessing E/W.",
       subtitle = "Students categorized by F&P levels (5th = T+, 6th = W+, 7th & 8th = Z)")
