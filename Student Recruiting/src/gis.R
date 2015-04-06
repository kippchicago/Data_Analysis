# Recruitement maps.
setwd("~/Dropbox (KIPP Chicago Schools)/Data Analysis/Student Recruiting")

require(ProjectTemplate)


load.project()

# glimpse(student.addresses)
# 
# 
# 
# student.addresses2 <- student.addresses %>%
#   mutate(complete_address=paste(address, city, st, zip)) %>%
#   filter(!grepl("po box", tolower(address)))
# 
# 
# max_batch<-nrow(student.addresses2)/100
# max_batch_split<-str_split(max_batch,pattern = "\\.")
# 
# batches<-c(rep((1:as.integer(max_batch_split[[1]][1])), each=100), 
#            rep(as.integer(max_batch_split[[1]][1])+1, times=as.integer(max_batch_split[[1]][2])))                  
# 
# assertthat::assert_that(length(batches)==nrow(student.addresses2))
# 
# student.addresses3 <- student.addresses2 %>%
#   mutate(batch=batches,
#          complete_address=gsub("(.+)\\s(APT.+|UNIT.+|\\#.+|FL\\s.+|BSMT.+|STE.+)\\s(CHICAGO.+)", "\\1 \\3", 
#                                     perl=TRUE,
#                                     complete_address
#                                )
#   )
# 
# # Geocoding fun ####
# 
# geocodeBatch <- function(address, bounding_box) {
#   #URL for batch requests
#   URL=paste("http://open.mapquestapi.com/geocoding/v1/batch?key=", "Fmjtd%7Cluub2huanl%2C20%3Do5-9uzwdz", 
#             "&location=", paste(address,collapse="&location="),sep = "") 
#   
#   URL <- gsub(" ", "+", URL)
#   data<-RCurl::getURL(URL)
#   data <- rjson::fromJSON(data)
#   
#   p<-sapply(data$results,function(x){
#     if(length(x$locations)==0){
#       c(NA,NA)
#     } else{
#       c(x$locations[[1]]$displayLatLng$lat, x$locations[[1]]$displayLatLng$lng)   
#     }})
#   
#   out_list<-t(p)
#   out<-data.frame(lat=out_list[,1], long=out_list[,2])  
# }
# 
# 
# batch_numbers<-unique(batches)
# 
# latlong_list<-lapply(batch_numbers,
#                      function(x){
#                        addr <- student.addresses3 %>% 
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
# stus_addr<-student.addresses3 %>%
#   mutate(lat=latlong_df$lat,
#          long=latlong_df$long)
# 
#save(stus_addr, file="data/mailers_geocoded.Rda")

load("data/mailers_geocoded.Rda")

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
  geom_point(data=stus_addr, 
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
  geom_point(data=stus_addr, 
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
  geom_point(data=stus_addr, 
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
  geom_point(data=stus_addr, 
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
  stat_density2d(data=stus_addr,
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

## Getting down to cencues block groups/blocks/tracks ###

census_blocks_shapefile<-readOGR("shapefiles/Census_Blocks/", 
                                 "Census Blocks")

census_blocks_shapefile<-spTransform(census_blocks_shapefile, 
                                     CRS("+proj=longlat +datum=WGS84"))

# let's get some shapefiles 
stus_sp <- na.exclude(stus_addr)

coordinates(stus_sp) = ~long+lat
proj4string(stus_sp)<- CRS("+proj=longlat +datum=WGS84")

str(stus_sp)

stus_blocked<-over(stus_sp, census_blocks_shapefile)

cbind(stus_sp, stus_blocked) %>%glimpse
  
