library(readxl)
library(bigrquery)
library(tidyverse)
library(silounloadr)

# Get exisiting col names (this should be verified each year)
col_names_ela <- get_parcc("cps_results_ela") %>% colnames()
col_names_math <- get_parcc("cps_results_math") %>% colnames()

#Read ELA from Excel
parcc_cps_ela <- read_excel(path = "PARCC_1617/excel_files/Assessment_PARCC_SchoolLevel_2017.xls",
           sheet = "PARCC ELA Results",
           skip = 2,
           col_names = col_names_ela
            )
#Read Math from Excel
parcc_cps_math <- read_excel(path = "PARCC_1617/excel_files/Assessment_PARCC_SchoolLevel_2017.xls",
                            sheet = "PARCC Math Results",
                            skip = 2,
                            col_names = col_names_math
)


#truncate and upload ela
bq_res <- insert_upload_job(
  project = "kipp-chicago-silo-2",
  dataset = "parcc",
  table = "cps_results_ela",
  value = parcc_cps_ela,
  write_disposition = "WRITE_TRUNCATE"
)

wait_for(bq_res)

#truncate and upload ela
bq_res <- insert_upload_job(
  project = "kipp-chicago-silo-2",
  dataset = "parcc",
  table = "cps_results_math",
  value = parcc_cps_math,
  write_disposition = "WRITE_TRUNCATE"
)

wait_for(bq_res)

# Done!
