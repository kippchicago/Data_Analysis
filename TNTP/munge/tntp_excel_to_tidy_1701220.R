library(xlsx)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(zoo)
library(ggplot2)



tntp_file <- "data/Fall 2017 KIPP Chicago_Teacher Survey School Sorter.xlsx"
# grab headers and make tidy
# headers<-read.xlsx(tntp_file,
#                    sheetIndex = 2,
#                         startRow = 1,
#                         endRow = 2,
#                         header = FALSE,
#                         stringsAsFactors = FALSE)

headers <- read_excel(tntp_file,
                      sheet =2,
                      range = cell_rows(1:2),
                      col_names = FALSE)



headers_t <- headers %>%
  t() %>%
  as_tibble() %>%
  rename(domain = V1, prompt = V2) %>%
  mutate(col_index = sprintf("X__%s", rownames(.)),
         domain = zoo::na.locf(domain, na.rm=FALSE),
         prompt = ifelse(str_detect(prompt, "(School Sorter)|(School Summary)"), NA, prompt))
# headers_t <- t(headers) %>%
#   as.data.frame(stringsAsFactors = FALSE) %>%
#   rename(domain = V1, prompt = V2) %>%
#   mutate(col_index = rownames(.),
#          domain = zoo::na.locf(domain, na.rm=FALSE),
#          prompt = ifelse(str_detect(prompt, "School Sorter|School Summary"), NA, prompt))

# grab column to use for informational purposes
# column_A<-read.xlsx(tntp_file,
#                     sheetIndex = 2,
#                    colIndex = 1,
#                    header = FALSE,
#                    stringsAsFactors = FALSE)

column_A <- read_excel(tntp_file,
                       sheet = 2,
                       range = cell_cols(1),
                       col_names = FALSE
                       )

# Get some info from column A like the first row with the word "Note"

first_foundation_row <- grep("Insight Report|School Sorter", column_A$X__1) + 2
note_row<-grep("Note", column_A$X__1) +1
last_school_row <- note_row - 2
last_foundation_row <- grep("^School$", column_A$X__1)


# get foundation data
# tntp_foundation <- read.xlsx(tntp_file,
#                             sheetIndex = 2,
#                    startRow = first_foundation_row,
#                    endRow = first_foundation_row + 2,
#                    header = FALSE,
#                    stringsAsFactors = FALSE)

tntp_foundation <- read_excel(tntp_file,
                                sheet = 2,
                                range = cell_rows(first_foundation_row:last_foundation_row),
                                col_names = FALSE)


tntp_foundation <- tntp_foundation %>%
  dplyr::filter(!str_detect(X__1, "Below")) %>%
  rename(school = X__1,
         region = X__2,
         grade_level = X__3) %>%
  gather(col_index, value, 4:ncol(tntp_foundation)) %>%
  left_join(headers_t, by = "col_index")


# get schools  data
# tntp_schools <-read.xlsx(tntp_file,
#                          sheetIndex = 2,
#                             startRow = last_foundation_row + 2,
#                             endRow = last_school_row,
#                             header = FALSE,
#                             stringsAsFactors = FALSE)

tntp_schools <- read_excel(tntp_file,
                           sheet = 2,
                           range = cell_rows((last_foundation_row + 2):last_school_row),
                           col_names = FALSE)

tntp_schools <- tntp_schools %>%
  rename(school = X__1,
         grade_level = X__2,
         region = X__3) %>%
  gather(col_index, value, 4:ncol(tntp_schools)) %>%
  left_join(headers_t, by = "col_index") %>%
  mutate(value = as.numeric(ifelse(value=="Top-Q", 1, value)))


# combine schools and foundation data into one data frame

tntp_f17 <- bind_rows(tntp_schools, tntp_foundation)

