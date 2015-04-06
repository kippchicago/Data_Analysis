map_all_silo<-map_all_silo %>% 
  mutate(SchoolName=ifelse(grepl("Ascend", SchoolName),
                           "KIPP Ascend College Prep",SchoolName
  ),
  Grade=ifelse(Grade=="K", 0, as.integer(Grade)),
  MeasurementScale=ifelse(grepl("General", MeasurementScale), 
                          "General Science",
                          MeasurementScale)
  ) %>%
  filter(MeasurementScale %in% c("Reading", "Mathematics", "General Science"))

