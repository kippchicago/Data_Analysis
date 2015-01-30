map_mv <- map_all_silo %>%
  mutate(Grade=ifelse(Grade=="K",0,as.integer(Grade))) %>%
  filter(!(TermName=="Fall 2012-2013" & TeacherName=="Bass, Jamelle")) %>%
  mapvizier
           

map_mv$mapData <-map_mv$mapData %>% 
  mutate(SchoolInitials=ifelse(grepl("A", SchoolInitials), 
                               "KACP", 
                               SchoolInitials)
  )