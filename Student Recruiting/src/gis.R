setwd("~/Dropbox (KIPP Chicago Schools)/Data Analysis/Student Recruiting")

require(ProjectTemplate)


load.project()

# sf<-read.dcf("config//sales_force.dcf") %>% as.data.frame(stringsAsFactors = FALSE)
# 
# 
# session <- rforcecom.login(sf$username, 
#                            sf$key, 
#                            sf$instanceURL, 
#                            sf$apiVersion)
# 
# stu_qry<-"
# SELECT  LastName,
#         FirstName,
#         MailingStreet,
#         MailingCity,
#         MailingState,
#         MailingPostalCode,
#         Expected_HS_Graduation__c 
# FROM Contact
# "
# 
# sf_alumni <- rforcecom.query(session, stu_qry)
# 
# alumni_addresses<-as.data.frame(lapply(sf_alumni, as.character ), stringsAsFactors=FALSE)
# 
# 
# 
# alumni_addresses.2 <- alumni_addresses %>% 
#   filter(!is.na(MailingStreet)) %>%
#   mutate(Cohort=lubridate::year(lubridate::ymd(Expected_HS_Graduation__c)),
#          complete_address=paste(MailingStreet, MailingCity, MailingState, MailingPostalCode),
#          complete_address=toupper(complete_address),
#          complete_address=gsub("(.+)\\s(APT.+|UNIT.+|\\#.+|FL\\s.+|BSMT.+|STE.+)\\s(CHICAGO.+)", "\\1 \\3", 
#                                perl=TRUE,
#                                complete_address
#                                ),
#         complete_address=stringr::str_replace_all(complete_address, "\\.", "")
#          ) %>%
#   filter(!grepl("PO BOX|P.O. BOX", complete_address))
#   
# 
# max_batch<-nrow(alumni_addresses.2)/100
# max_batch_split<-stringr::str_split(max_batch,pattern = "\\.")
# 
# batches<-c(rep((1:as.integer(max_batch_split[[1]][1])), each=100), 
#            rep(as.integer(max_batch_split[[1]][1])+1, times=as.integer(max_batch_split[[1]][2])))                  
# 
# assertthat::assert_that(length(batches)==nrow(alumni_addresses.2))
# 
# # Must remove Unit numbers, floor number, bsmt designations and the like from address fields
# alumni_addresses.3 <- alumni_addresses.2 %>%
#   mutate(batch=batches)
# 
# # Geocoding fun ####
# 
# latlong_list<-lapply(alumni_addresses.3$complete_address, ggmap::geocode)
# 
# latlong_df<-rbind_all(latlong_list)
# 
# 
# alumni_addr<-alumni_addresses.3 %>%
#   mutate(lat=latlong_df$lat,
#          long=latlong_df$lon)
# 
# save(alumni_addr, file = "data//alumni_geocoded.Rda")



# glimpse(mailer.addresses)
# 
# 
# 
# mailer.addresses2 <- mailer.addresses %>%
#   mutate(complete_address=paste(address, city, st, zip)) %>%
#   filter(!grepl("po box", tolower(address)))
# 
# 
# max_batch<-nrow(mailer.addresses2)/100
# max_batch_split<-str_split(max_batch,pattern = "\\.")
# 
# batches<-c(rep((1:as.integer(max_batch_split[[1]][1])), each=100), 
#            rep(as.integer(max_batch_split[[1]][1])+1, times=as.integer(max_batch_split[[1]][2])))                  
# 
# assertthat::assert_that(length(batches)==nrow(mailer.addresses2))
# 
# # Must remove Unit numbers, floor number, bsmt designations and the like from address fields
# mailer.addresses3 <- mailer.addresses2 %>%
#   mutate(batch=batches,
#          complete_address=gsub("(.+)\\s(APT.+|UNIT.+|\\#.+|FL\\s.+|BSMT.+|STE.+)\\s(CHICAGO.+)", "\\1 \\3", 
#                                     perl=TRUE,
#                                     complete_address
#                                )
#   )
# 
# # Geocoding fun ####
# 

# 
# 
# batch_numbers<-unique(batches)
# 
# latlong_list<-lapply(batch_numbers,
#                      function(x){
#                        addr <- mailer.addresses3 %>% 
#                          filter(batch==x) %>% 
#                          select(complete_address)
#                        
#                        message(paste("Geocoding batch #:", x))
#                        
#                        out<-geocodeBatch(addr$complete_address)
#                        
#                        out
#                      })
# 
# latlong_df<-rbind_all(latlong_list)
# 
# 
# mailer_addr<-mailer.addresses3 %>%
#   mutate(lat=latlong_df$lat,
#          long=latlong_df$long)
#save(mailer_addr, file="data/mailers_geocoded.Rda")

#load("data/mailers_geocoded.Rda")


# Here's where we do the map magic ####

chicago<-get_map(location = c(lon=mean(latlong_df$long, na.rm = TRUE),
                              lat=mean(latlong_df$lat, na.rm = TRUE)), 
                 zoom=11, 
                 maptype = 'toner-lite', 
                 source = "stamen")

chicago_map_base <- ggmap(chicago,
                          base_layer = ggplot(data=stus_addr,
                                              aes(x=long, y=lat) 
                          ),
                          extent = "normal", 
                          maprange = FALSE
                          )


chicago_map<-chicago_map_base + 
  geom_point(data=mailer_addr, 
             aes(x=long,
                 y=lat
             ),
             color="orange",
             size=1,
             alpha=.4,
             )   +
  coord_map(projection="mercator", 
            xlim=c(attr(chicago, "bb")$ll.lon, 
                   attr(chicago, "bb")$ur.lon),
            ylim=c(attr(chicago, "bb")$ll.lat,
                   attr(chicago, "bb")$ur.lat))


chicago_map


# north lawndale ####

nl<-get_map(location = "1616 S Avers Ave, Chicago, IL", 
                 zoom=14, 
                 maptype = 'toner-lite', 
                 source = "stamen")

nl_map_base <- ggmap(nl)

nl_map<-nl_map_base + 
  geom_point(data=mailer_addr, 
             aes(x=long,
                 y=lat
             ),
             color="orange",
             size=2,
             alpha=.4,
  )  


nl_map


# austin ####

austin<-get_map(location = "  4818 W Ohio St, Chicago, IL 60644", 
            zoom=14, 
            maptype = 'toner-lite', 
            source = "stamen")

austin_map_base <- ggmap(austin)

austin_map<-austin_map_base + 
  geom_point(data=mailer_addr, 
             aes(x=long,
                 y=lat
             ),
             color="orange",
             size=2,
             alpha=.4,
  )  


austin_map


# englewood ####

englewood<-get_map(location = "5515 S Lowe Ave, Chicago, IL 60621", 
                zoom=14, 
                maptype = 'toner-lite', 
                source = "stamen")

englewood_map_base <- ggmap(englewood)

englewood_map<-englewood_map_base + 
  geom_point(data=mailer_addr, 
             aes(x=long,
                 y=lat
             ),
             color="orange",
             size=2,
             alpha=.4,
  )  


englewood_map


pdf("graphs/mailing_maps.pdf", onefile=TRUE, height=11, width=8.5)
chicago_map + ggtitle("Chicago")
nl_map + ggtitle("North Lawndale")
austin_map + ggtitle("Austin")
englewood_map + ggtitle("Englewood")
dev.off()


# Heatmaps ####


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



chicago_2<-get_map(location=c(-87.9, 41.65, -87.5, 42.05), 
                   zoom=14, 
                   maptype="toner-lite", 
                   source="stamen")


chi_map_2<-ggmap(chicago_2, 
                 base_layer = ggplot(data=municipalities.df,
                                     aes(x=long, y=lat) 
                 ),
                 extent = "normal", 
                 maprange = FALSE
)



# Pct AA ####




p_hm <- chi_map_2 + 
  geom_polygon(data=community_areas.df,
               aes(x=long, 
                   y=lat, 
                   group=group),
               color="gray50",
               size=2,
               fill=NA) +
#   geom_density2d(data=stus_addr,
#                  aes(x=long,
#                      y=lat), 
#                  size=.5
#                  ) + 
  stat_density2d(data=mailer_addr,
                 aes(x=long,
                     y=lat,
                     fill=..level..,
                     alpha=..level..),
                 size=0.01,
                 bins=32,
                 geom="polygon"
                ) + 
  scale_fill_gradient("Population Density",low="green", high="red") +
  scale_alpha(range=c(0,0.3), guide=FALSE) +
  coord_map(projection = "mercator",
            xlim=c(attr(chicago_2, "bb")$ll.lon, 
                   attr(chicago_2, "bb")$ur.lon),
            ylim=c(attr(chicago_2, "bb")$ll.lat,
                   attr(chicago_2, "bb")$ur.lat
            )
  ) + 
  theme_nothing()

  
p_hm


cairo_pdf("graphs/chi_pop_density.pdf",
          height=48,
          width=36)
p_hm + 
  ggtitle("Population Density of Families with 5th-6th graders")
dev.off()

## Getting down to cencues block groups/blocks/tracks ####

census_blocks_shapefile<-readOGR("shapefiles/Census_Blocks/", 
                                 "Census Blocks")

census_blocks_shapefile<-spTransform(census_blocks_shapefile, 
                                     CRS("+proj=longlat +datum=WGS84"))



# let's get some shapefiles 
mailer_sp <- na.exclude(mailer_addr)

coordinates(mailer_sp) = ~long+lat
proj4string(mailer_sp)<- CRS("+proj=longlat +datum=WGS84")

#str(mailer_sp)

mailer_blocked<-over(mailer_sp, census_blocks_shapefile)

mailer_blocked2<-cbind(mailer_sp, mailer_blocked) 


# Austin Redux ####




census_blocks_shapefile@data$id <- rownames(census_blocks_shapefile@data)
census_blocks <- fortify(census_blocks_shapefile, region="id")
census_blocks.df<-merge(census_blocks, 
                        census_blocks_shapefile, 
                        by="id") %>%
  arrange(id, order) %>%
  as.data.frame


zips<-c(60624, 60644, 60651)

austin<-get_map(location = "  4818 W Ohio St, Chicago, IL 60644", 
                zoom=14, 
                maptype = 'toner-lite', 
                source = "stamen")

census_filtered<-census_blocks.df%>%
  filter(as.integer(BLOCK_ZIP) %in% zips)

austin_map_base<-ggmap(austin, 
                 base_layer = ggplot(data=municipalities.df,
                                     aes(x=long, y=lat) 
                                     ),
                 extent = "normal", 
                 maprange = FALSE
                 )


block_centers <- census_blocks.df %>%
  group_by(CENSUS_T_1) %>%
  mutate(max_long=max(long),
         min_long=min(long),
         max_lat=max(lat),
         min_lat=min(lat)
         ) %>%
  select(long=BLOCK_CE_2, 
         lat=BLOCK_CE_3, 
         block_name=CENSUS_T_1, 
         zip=BLOCK_ZIP,
         max_long,
         min_long,
         max_lat,
         min_lat
  ) %>%
  unique

austin_map<-austin_map_base + 
  geom_polygon(data=census_blocks.df %>%
                 filter(as.integer(BLOCK_ZIP) %in% zips),
               aes(x=long, 
                   y=lat, 
                   group=group),
               color="hotpink",
               size=1,
               fill=NA,
               alpha=.3) + 
  geom_text(data=block_centers %>%
                 filter(as.integer(zip) %in% zips),
               aes(x=long, 
                   y=lat,
                   label=block_name),
               color="hotpink",
               size=1,
               alpha=1) + 
  geom_point(data=mailer_blocked2 %>%
               filter(as.integer(BLOCK_ZIP) %in% zips), 
             aes(x=long,
                 y=lat
             ),
             color="orange",
             size=2,
             alpha=.4,
  )  +
  coord_map(projection = "mercator",
            xlim=c(attr(austin, "bb")$ll.lon, 
                   attr(austin, "bb")$ur.lon),
            ylim=c(attr(austin, "bb")$ll.lat,
                   attr(austin, "bb")$ur.lat
            )
  ) + 
  theme_nothing()



austin_map

# Zooming in ###

austin_blocks <- block_centers %>%
  filter(as.integer(zip) %in% zips) %>%
  arrange(block_name)



n_per_batch <- 4
n_batches<-nrow(austin_blocks)/n_per_batch %>% trunc
n_last_batch <- nrow(austin_blocks) %% n_per_batch # number of cencus blocs mod 4

batch_numbers<-rep(1:n_batches, each=n_per_batch)
if(n_last_batch>0) batch_numbers <- c(batch_numbers, rep(n_batches+1, times = n_last_batch))



austin_blocks$batch<- batch_numbers

latlon <- austin_blocks %>%
  filter(batch==20) %>%
  select(min_long, max_long, min_lat, max_lat) %>%
  ungroup %>%
  summarize(min_long=min(min_long), min_lat=min(min_lat), max_long=max(max_long), max_lat=max(max_lat))

austin_zoomed<-get_map(location = c(latlon$min_long, latlon$min_lat, latlon$max_long,latlon$max_lat), 
                zoom=18, 
                maptype = 'toner', 
                source = "stamen")



austin_filtered <- austin_blocks %>% filter(batch==20)

census_blocks_filtered.df<-census_blocks.df %>%
  filter(CENSUS_T_1 %in% austin_filtered$block_name)

mailers_filtered<-mailer_blocked2 %>%
  filter(CENSUS_T_1 %in% austin_filtered$block_name)




austin_zoomed_map_base<-ggmap(austin_zoomed, 
                       base_layer = ggplot(data=municipalities.df,
                                           aes(x=long, y=lat) 
                       ),
                       extent = "normal", 
                       maprange = FALSE
)

census_blocks_zoomed.df <- filter(census_blocks.df, 
                                  CENSUS_T_1 %in% austin_filtered$block_name
                                      )

austin_map<-austin_zoomed_map_base + 
  geom_polygon(data=census_blocks_zoomed.df,
               aes(x=long, 
                   y=lat, 
                   group=group),
               color="hotpink",
               size=1,
               fill=NA,
               alpha=.3) + 
  geom_text(data=austin_filtered,
            aes(x=long, 
                y=lat,
                label=block_name),
            color="hotpink",
            size=8,
            alpha=1) + 
  geom_point(data=mailers_filtered %>%
               filter(CENSUS_T_1 %in% austin_filtered$block_name), 
             aes(x=long,
                 y=lat
             ),
             color="orange",
             size=4,
             alpha=.8,
  )  +
  coord_map(projection = "mercator",
            xlim=c(attr(austin_zoomed, "bb")$ll.lon, 
                   attr(austin_zoomed, "bb")$ur.lon),
            ylim=c(attr(austin_zoomed, "bb")$ll.lat,
                   attr(austin_zoomed, "bb")$ur.lat
            )
  ) + 
  theme_nothing()



austin_map


# one pager ###





