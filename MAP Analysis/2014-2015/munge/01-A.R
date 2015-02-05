map_mv <- map_all_silo %>%
  mutate(Grade=ifelse(Grade=="K",0,as.integer(Grade))) %>%
  filter(!(TermName=="Fall 2012-2013" & TeacherName=="Bass, Jamelle")) %>%
  mapvizier
           

map_mv$mapData <-map_mv$mapData %>% 
  mutate(SchoolInitials=ifelse(grepl("A", SchoolInitials), 
                               "KACP", 
                               SchoolInitials)
  )

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

map_homerooms<-map_homerooms %>%
  mutate(StudentID=as.numeric(StudentID))