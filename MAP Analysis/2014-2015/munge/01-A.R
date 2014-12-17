map_mv<-mapvizier(map_all)

map_mv$mapData <-map_mv$mapData %>% 
  mutate(SchoolInitials=ifelse(grepl("A", SchoolInitials), 
                               "KACP", 
                               SchoolInitials)
  )