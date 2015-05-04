# HSR preprocessing script for school 2014-15
# each separate school data.table has national and regional data included in it.
# We need to strip out the national and regional data into separate data frames
# and Measure.Names need to be made common across all categories (rather than
# School % Lickert 1, National % Lickert 2, Region % Lickert 4, etc.).

# Strip out regional and national from each data.table
require(readr)
hsr_data<-read_csv("data/HSR_schools_teachers_1415.csv")

hsr_list <-list()
hsr_list$teachers<-read_csv("data/HSR_schools_teachers_1415.csv")
hsr_list$staff<-read_csv("data/HSR_schools_staff_1415.csv")
hsr_list$students<-read_csv("data/HSR_schools_students_1415.csv")
hsr_list$parents<-read_csv("data/HSR_schools_parents_1415.csv")

hsr_data<-rbind_all(hsr_list)



#kap has fewer rows from the middle schools for some reason.  So get regional adn
#national from KAMS

hsr_national <- hsr_data %>%
  filter(grepl("National", measure_names),
         grepl("Primary", school)) %>%
  mutate(school="KIPP Network",
         measure_names = stringr::str_replace(measure_names, "National ", ""))

hsr_region <- hsr_data %>%
  filter(grepl("Region", measure_names),
         grepl("Primary", school)) %>%
  mutate(school="KIPP Chicago",
         measure_names = stringr::str_replace(measure_names, "Regional ", ""),
         measure_names = stringr::str_replace(measure_names, "Region ", ""))

hsr_schools <- hsr_data %>%
  filter(grepl("School", measure_names)) %>%
  mutate(measure_names = stringr::str_replace(measure_names, "School ", ""))


hsr<-rbind_list(hsr_national, hsr_region, hsr_schools) %>%
  mutate(measure_names=ifelse(grepl("Likert 5", measure_names), "Strongly Agree", measure_names),
         measure_names=ifelse(grepl("Likert 4", measure_names), "Agree", measure_names),
         measure_names=ifelse(grepl("Likert 3", measure_names), "Neutral", measure_names),
         measure_names=ifelse(grepl("Likert 2", measure_names), "Disagree", measure_names),
         measure_names=ifelse(grepl("Likert 1", measure_names), "Strongly Disagree", measure_names),
         measure_names=ifelse(grepl("ositive", measure_names), "Pct Positive", measure_names),
         measure_names=ifelse(grepl("Responded", measure_names), "Num Responded", measure_names))

