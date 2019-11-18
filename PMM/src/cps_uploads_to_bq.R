# SQRP ####

setwd("/Users/chaid/Dropbox (KIPP Chicago Schools)/Data Analysis/PMM/")


sqrp_files <- list.files("excel files/cps_data_releases/SQRP",
                         full.names = TRUE)

col_data <- frame_data(
               ~col_name, ~col_type,
               "schoolid", "numeric",
               "schoolname", "text",
               "network",  "text",

               "total_points", "numeric",
               "rating", "text",
               "accountability_status", "text",

               "growth_all_score_reading", "numeric",
               "growth_all_partic_reading", "numeric",
               "growth_all_points_reading", "numeric",
               "growth_all_weight_reading", "numeric",

               "growth_all_score_math", "numeric",
               "growth_all_partic_math", "numeric",
               "growth_all_points_math", "numeric",
               "growth_all_weight_math", "numeric",

               "pct_me_score", "numeric",
               "pct_me_partic", "numeric",
               "pct_me_points", "numeric",
               "pct_me_weight", "numeric",

               "growth_aa_score_reading", "numeric",
               "growth_aa_partic_reading", "numeric",
               "growth_aa_points_reading", "numeric",
               "growth_aa_weight_reading", "numeric",

               "growth_hisp_score_reading", "numeric",
               "growth_hisp_partic_reading", "numeric",
               "growth_hisp_points_reading", "numeric",
               "growth_hisp_weight_reading", "numeric",

               "growth_ell_score_reading", "numeric",
               "growth_ell_partic_reading", "numeric",
               "growth_ell_points_reading", "numeric",
               "growth_ell_weight_reading", "numeric",

               "growth_dl_score_reading", "numeric",
               "growth_dl_partic_reading", "numeric",
               "growth_dl_points_reading", "numeric",
               "growth_dl_weight_reading", "numeric",

               "growth_aa_score_math", "numeric",
               "growth_aa_partic_math", "numeric",
               "growth_aa_points_math", "numeric",
               "growth_aa_weight_math", "numeric",

               "growth_hisp_score_math", "numeric",
               "growth_hisp_partic_math", "numeric",
               "growth_hisp_points_math", "numeric",
               "growth_hisp_weight_math", "numeric",

               "growth_ell_score_math", "numeric",
               "growth_ell_partic_math", "numeric",
               "growth_ell_points_math", "numeric",
               "growth_ell_weight_math", "numeric",

               "growth_dl_score_math", "numeric",
               "growth_dl_partic_math", "numeric",
               "growth_dl_points_math", "numeric",
               "growth_dl_weight_math", "numeric",

               "attain_38_score_reading", "numeric",
               "attain_38_partic_reading", "numeric",
               "attain_38_points_reading", "numeric",
               "attain_38_weight_reading", "numeric",

               "attain_38_score_math", "numeric",
               "attain_38_partic_math", "numeric",
               "attain_38_points_math", "numeric",
               "attain_38_weight_math", "numeric",

               "attain_2_score_reading",  "numeric",
               "attain_2_partic_reading", "numeric",
               "attain_2_points_reading", "numeric",
               "attain_2_weight_reading", "numeric",

               "attain_2_score_math", "numeric",
               "attain_2_partic_math", "numeric",
               "attain_2_points_math", "numeric",
               "attain_2_weight_math", "numeric",

               "access_score", "numeric",
               "access_partic", "numeric",
               "access_points", "numeric",
               "access_weight", "numeric",

               "ada_score", "numeric",
               "ada_points", "numeric",
               "ada_weight", "numeric",

               "mvms_score", "text",
               "mvms_points", "numeric",
               "mvms_weight", "numeric",

               "dqi_score", "numeric",
               "dqi_points", "numeric",
               "dqi_weight", "numeric"
)


sqrp_data <- sqrp_files %>%
  map(~read_excel(path = ., sheet = 2,
                  skip = 4,
                  col_names = col_data$col_name,
                  col_types = col_data$col_type
                  )
  )




sqrp_data[[1]]$sy <- "SY2016-2017"
sqrp_data[[2]]$sy <- "SY2014-2015"
sqrp_data[[3]]$sy <- "SY2015-2016"

sqrp_combined <- bind_rows(sqrp_data) %>%
  mutate(schoolid = as.integer(schoolid)) %>%
  filter(!is.na(schoolid))


bigrquery::insert_upload_job("kipp-chicago-silo-2", 'cps', 'sqrp', sqrp_combined)

# Race/Ethnicity ####


race_files <- list.files("excel files/cps_data_releases/race",
                         full.names = TRUE)

race_col_data <- frame_data(
  ~col_name, ~col_type,
  "network",  "text",
  "schoolid", "text",
  "schoolname", "text",

  "n", "numeric",

  "n_white", "numeric",
  "pct_white", "numeric",

  "n_aa", "numeric",
  "pct_aa", "numeric",

  "n_asian_pi", "numeric",
  "pct_asian_pi", "numeric",

  "n_native_amer_alaskan", "numeric",
  "pct_native_amer_alaskan", "numeric",

  "n_hisp", "numeric",
  "pct_hisp", "numeric",

  "n_multi", "numeric",
  "pct_multi", "numeric",

  "n_asian", "numeric",
  "pct_asian", "numeric",

  "n_hawaiian_pi", "numeric",
  "pct_hawaiian_pi", "numeric",

  "n_not_avail", "numeric",
  "pct_not_avail", "numeric"

)


race_data <- race_files %>%
  map(~read_excel(path = ., sheet = 3,
                  skip = 2,
                  col_names = race_col_data$col_name,
                  col_types = race_col_data$col_type)
  )




race_data[[1]]$sy <- "SY2016-2017"
race_data[[2]]$sy <- "SY2014-2015"
race_data[[3]]$sy <- "SY2015-2016"

race_combined <- bind_rows(race_data) %>%
  mutate(schoolid = as.integer(schoolid)) %>%
  filter(!is.na(schoolid))


bigrquery::insert_upload_job("kipp-chicago-silo-2", 'cps', 'race_ethnicity', race_combined)

#Other demo graphics (ELL, DL, FRM)

ell_dl_frm_files <- list.files("excel files/cps_data_releases/ell_dl_frl/",
                         full.names = TRUE)

ell_dl_frm_col_data <- frame_data(
  ~col_name, ~col_type,
  "network",  "text",
  "schoolid", "text",
  "schoolname", "text",

  "n", "numeric",

  "n_ell", "numeric",
  "pct_ell", "numeric",

  "n_sped", "numeric",
  "pct_sped", "numeric",

  "n_frm", "numeric",
  "pct_frm", "numeric"
)


ell_dl_frm_data <- ell_dl_frm_files %>%
  map(~read_excel(path = ., sheet = 2,
                  skip = 2,
                  col_names = ell_dl_frm_col_data$col_name,
                  col_types = ell_dl_frm_col_data$col_type)
  )




ell_dl_frm_data[[1]]$sy <- "SY2014-2015"
ell_dl_frm_data[[2]]$sy <- "SY2016-2017"
ell_dl_frm_data[[3]]$sy <- "SY2015-2016"

ell_dl_frm_combined <- bind_rows(ell_dl_frm_data) %>%
  mutate(schoolid = as.integer(schoolid)) %>%
  filter(!is.na(schoolid))


bigrquery::insert_upload_job("kipp-chicago-silo-2", 'cps', 'ell_dl_frm', ell_dl_frm_combined)

# suspeneions ####

susps_files <- list.files("excel files/cps_data_releases/suspensions/",
                               full.names = TRUE)

susps_col_data <- frame_data(
  ~col_name, ~col_type,
  "schoolid", "text",
  "schoolname", "text",
  "network",  "text",
  "sy", "text",
  "period", "text",

  "n_misconducts", "numeric",
  "n_misconducts_g12", "numeric",
  "n_misconducts_g34", "numeric",
  "n_misconducts_g56", "numeric",

  "n_suspensions", "numeric",
  "pct_misconduct_to_susps", "numeric",

  "n_iss",   "numeric",
  "pct_misconduct_to_iss",  "numeric",
  "iss_per_100", "numeric",
  "n_unique_students_iss", "numeric",
  "pct_unique_students_iss", "numeric",
  "avg_length_iss", "numeric",

  "n_oss",  "numeric",
  "pct_misconduct_to_oss",  "numeric",
  "oss_per_100", "numeric",
  "n_unique_students_oss", "numeric",
  "pct_unique_students_oss", "numeric",
  "avg_length_oss", "numeric",

  "n_police",  "numeric",
  "pct_misconduct_to_police",  "numeric",
  "police_per_100", "numeric",
  "n_unique_students_police", "numeric",
  "pct_unique_students_police", "numeric",

  "n_expelled", "numeric",
  "expulsions_per_100", "numeric"

)


susps_data <- susps_files %>%
  map(~read_excel(path = ., sheet = 1,
                  na = "--",
                  skip = 1,
                  col_names = susps_col_data$col_name,
                  col_types = susps_col_data$col_type
                  )
  )


susps_combined <- bind_rows(susps_data) %>%
  mutate(schoolid = as.integer(schoolid),
         sy = sprintf("SY%s", sy)) %>%
  filter(!is.na(schoolid))


bigrquery::insert_upload_job("kipp-chicago-silo-2", 'cps', 'suspensions', susps_combined)
