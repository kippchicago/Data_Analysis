# Script to extract and save student level PARCC data from CPS spreadsheets

require(readxl)
require(purrr)
require(dplyr)
require(stringr)
require(tidyr)


files_reading<-list.files("./results_files/Excel/",
                          pattern = "\\w+ELA\\w+.xlsx",
                          full.names = TRUE,
                          recursive = TRUE)
files_math<-list.files("./results_files/Excel/",
                       pattern = "\\w+(MATH|Math)\\w+.xlsx",
                       full.names = TRUE,
                       recursive = TRUE)


# quick function to fix column names
fix_column_names <- . %>%
  names() %>%
  str_to_lower() %>%
  str_replace_all("\\s", "_") %>%
  str_replace_all("\\(", "_") %>%
  str_replace_all("[\\?\\)]", "")


parcc_ela <- files_reading %>%
  purrr::map(read_excel) %>%
  bind_rows

names(parcc_ela) <- fix_column_names(parcc_ela)

parcc_ela <- parcc_ela %>%
  mutate(grade = as.integer(str_extract(test_code, "\\d+")),
         subject = "ela")

parcc_math <- files_math %>%
  purrr::map(read_excel) %>%
  bind_rows

names(parcc_math) <- fix_column_names(parcc_math)

parcc_math <- parcc_math %>%
  mutate(grade = as.integer(str_extract(test_code, "\\d+")),
         subject="math")

# tidy both

parcc_ela_long <- parcc_ela %>%
  rename(overall_scale_score = ela_overall_scale_score,
         performance_level = ela_performance_level) %>%
  gather(key = metric, value = value, reading_scale_score:writing_conventions)

parcc_math_long <- parcc_math %>%
  rename(overall_scale_score = math_overall_scale_score,
         performance_level = math_performance_level) %>%
  gather(key = metric, value = value, math_major_content:math_modeling)

parcc <- bind_rows(parcc_math_long, parcc_ela_long)

save(parcc, file="data/parcc_student_data.rda")
