require(tm)
require(dplyr)
require(purrr)
source('lib/get_parcc_state_distrcit_data.R')


# create reader function
pdf_reader <- readPDF(engine = "xpdf",
                      control  = list(text = "-table"))



#get file lists

files<-list.files(path = "./results_files/PDFs/KAP_KAMS/",
           pattern = "Roster",
           full.names = TRUE)

subjects <- str_match(files, "Mathematics|ELA") %>%
  as.vector

grades <- str_match(files,"Grade_\\d") %>%
  str_extract("\\d") %>%
  as.integer()

subject_grade_paths <-
  data_frame(subject = subjects,
             grade = grades,
             path = files
             )

parsed_pdfs <- list()
parsed_pdfs <- files %>%
  purrr::map(~pdf_reader(elem = list(uri = .),
                  language = "English",
                  id="1" )) %>%
 purrr::map("content")

subject_grade_data<-subject_grade_paths %>%
  mutate(data = parsed_pdfs)


parcc_benchmarks <- subject_grade_data %>%
  split(., 1:nrow(.)) %>%
  purrr::map_df(~extract_parcc_bechmarks(data = .$data[[1]],
                               subject = .$subject,
                               grade = .$grade))

save(parcc_benchmarks, file = "data/parcc_benchmarks.rda")
