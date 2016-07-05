# Pull MAP data for loading into PowerSchool

require(ProjectTemplate)
load.project()



# subjects
subjs <- c("Reading", "Mathematics", "General Science")
current_term <- "Spring 2015-2016"
# Change grade "K" to 0 and recast Grade as integer.
map_data<- map_all_silo %>%
  mutate(Grade=as.integer(ifelse(Grade=="K", 0, Grade)),
         MeasurementScale=ifelse(MeasurementScale=="Science - General Science",
                                 "General Science",
                                 MeasurementScale),
         season = stringr::str_extract(TermName, "Fall|Winter|Spring"),
         sy = stringr::str_extract(TermName, "\\d{4}-\\d{4}")
  ) %>%
  filter(MeasurementScale %in% subjs,
         TermName == current_term) %>%
  select(student_number = StudentID,
         test_date = TestStartDate,
         grade = Grade,
         subject = MeasurementScale,
         season_3= season,
         year_3 = sy,
         rit_1 = TestRITScore,
         pctl_1 = TestPercentile
         )

  names(map_data) <- tolower(names(map_data))
  # write to csvs by subject :::
  subjs %>%
    purrr::walk(~ map_data %>%
          filter(subject==.x) %>%
          readr::write_csv(sprintf("reports/map_ps_upload_%s.csv", .x))
    )
