# Munge student and mailer data
student_addr<-stus %>%
  select(first=first_name,
         last=last_name, 
         grade=grade_level,
         address=street,
         lat=lat.x,
         long=lon.x
         ) %>%
  mutate(type="KIPPster",
         cohort=NA) 


mailer_addr_selected<-mailer_addr %>%
  select(first,
         last,
         address,
         lat,
         long) %>%
  mutate(grade=NA,
         cohort=NA
         type="Postcard")


alumni_addr_selected<-alumni_addr %>%
  select(first=FirstName,
         last=LastName,
         address=complete_address,
         lat,
         long,
         cohort=Cohort) %>%
  mutate(grade=NA,
         type="Alumni")


combined_addresses<-rbind_list(student_addr, mailer_addr_selected, alumni_addr_selected) %>%
  filter(!is.na(lat), !is.na(long))
  

# load shapefiles ####
# community areas ####

# get cummunity areas ####
community_areas_shapefile<-readOGR("shapefiles/CommAreas/", 
                                   "CommAreas")

community_areas_shapefile<-spTransform(community_areas_shapefile, 
                                       CRS("+proj=longlat +datum=WGS84"))
# prep community areas for ggplot

community_areas_shapefile@data$id <- rownames(community_areas_shapefile@data)
community_areas <- fortify(community_areas_shapefile, region="id")
community_areas.df<-merge(community_areas, 
                          community_areas_shapefile, 
                          by="id") %>%
  arrange(id, order) %>%
  as.data.frame

# add_municipalities
municipalities_shapefile<-readOGR("shapefiles/Municipalities/", 
                                  "Municipalities")

municipalities_shapefile<-spTransform(municipalities_shapefile, 
                                      CRS("+proj=longlat +datum=WGS84"))
# prep community areas for ggplot

municipalities_shapefile@data$id <- rownames(municipalities_shapefile@data)
municipalities <- fortify(municipalities_shapefile, region="id")
municipalities.df<-merge(municipalities, 
                         municipalities_shapefile, 
                         by="id") %>%
  arrange(id, order) %>%
  as.data.frame


# assign addresses to community areas to aid in subsetting 
combined_sp <- combined_addresses %>% select(long, lat)%>% as.data.frame
coordinates(combined_sp) <- ~long+lat
proj4string(combined_sp)<- CRS("+proj=longlat +datum=WGS84")

cas_overlaid<-over(combined_sp, community_areas_shapefile)

combined_addresses$community_area<- cas_overlaid$COMMUNITY


