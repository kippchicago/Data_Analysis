create_sqrp_map <- function(gg_map, 
                            neighborhood_name,
                            vjust_adj=0.1,
                            hjust_adj=0,
                            ...){
  
  nn_caps<-toupper(neighborhood_name)
  
  school<-switch(nn_caps,
                 "AUSTIN" = "Create",
                 "NORTH LAWNDALE" = "Ascend",
                 "ENGLEWOOD" = "Bloom")
  schools <- kcs_schools_location %>%
    filter(grepl(school, schoolname))
  
  neighborhood_map <- ggmap(gg_map, 
                             base_layer=ggplot(data=community_areas.df,
                                               aes(x=long, y=lat)),
                             extent="normal",
                             maprange=FALSE)
  
  
  neighborhood_map<- neighborhood_map + 
    geom_polygon(data=community_areas.df %>% 
                   filter(COMMUNITY!=nn_caps),
                 aes(x=long, y=lat, group=COMMUNITY), 
                 fill="darkgray",
                 size=2,
                 #color='orange3', 
                 alpha=.8) +
    geom_polygon(data=community_areas.df %>% 
                   filter(COMMUNITY==nn_caps),
                 aes(x=long, y=lat, group=COMMUNITY), 
                 fill=NA,
                 color='orange3'
    ) +
    geom_point(data=sqrp %>% 
                 filter(community_area==neighborhood_name), 
               aes(x=lon,
                   y=lat,
                   color=level
               ),
               size=3,
               alpha=1
    ) + 
    geom_point(data=schools,
               aes(x=lon,
                   y=lat,
                   color=level
               ),
               size=4
    ) +
    geom_point(data=schools,
               aes(x=lon,
                   y=lat
               ),
               size=5,
               color="darkorange",
               shape=21,
    ) +
    geom_text(data=schools,
              aes(x=lon,
                  y=lat,
                  label=schoolname,
                  color=level
              ),
              size=4,
              hjust=hjust_adj,
              vjust=vjust_adj
    ) +
    scale_color_manual(values = c("gray", "forestgreen")) +
    coord_map(projection="mercator", 
              xlim=c(attr(gg_map, "bb")$ll.lon, 
                     attr(gg_map, "bb")$ur.lon),
              ylim=c(attr(gg_map, "bb")$ll.lat,
                     attr(gg_map, "bb")$ur.lat)) +
    theme(legend.direction="horizontal")
  
  neighborhood_map
 
}
