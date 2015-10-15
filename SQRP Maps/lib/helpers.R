create_sqrp_map <- function(gg_map,
                            neighborhood_name,
                            fill_column="level",
                            text_size=4,
                            vjust_adj=0.1,
                            hjust_adj=0,
                            show_1s = FALSE,
                            ...){



  nn_caps<-toupper(neighborhood_name)

  kipp_neighborhood<-all(nn_caps %in% c("AUSTIN",
                                    "NORTH LAWNDALE",
                                    "ENGLEWOOD"))
  if(kipp_neighborhood){
    school<-switch(nn_caps,
                   "AUSTIN" = "Create",
                   "NORTH LAWNDALE" = "Ascend",
                   "ENGLEWOOD" = "Bloom")
    schools <- kcs_schools_location %>%
      filter(grepl(school, schoolname))
  }

  neighborhood_map <- ggmap(gg_map,
                             base_layer=ggplot(data=municipalities.df,
                                               aes(x=long, y=lat)),
                             extent="normal",
                             maprange=FALSE)


  neighborhood_map<- neighborhood_map +
    geom_polygon(data=community_areas.df %>%
                   filter(!COMMUNITY%in%nn_caps),
                 aes(x=long, y=lat, group=COMMUNITY),
                 fill="darkgray",
                 size=2,
                 #color='orange3',
                 alpha=.8) +
    geom_polygon(data=municipalities.df,
                 aes(x=long, y=lat, group=group),
                 fill="darkgray",
                 size=2,
                 #color='orange3',
                 alpha=.8) +
    geom_polygon(data=community_areas.df %>%
                   filter(COMMUNITY %in% nn_caps),
                 aes(x=long, y=lat, group=COMMUNITY),
                 fill=NA,
                 color='orange3',
                 size=2
    ) +
    geom_point(data=sqrp %>%
                 filter(community_area %in% neighborhood_name),
               aes_string(x="lon",
                   y="lat",
                   fill=fill_column
               ),
               color="black",
               size=3,
               alpha=1,
               shape=21
    )

  if(kipp_neighborhood){
    neighborhood_map <- neighborhood_map +
      geom_point(data=schools,
                 aes_string(x="lon",
                     y="lat",
                     fill=fill_column
                 ),
                 size=4,
                 shape=21
      ) +
      geom_point(data=schools,
                 aes(x=lon,
                     y=lat
                 ),
                 size=5,
                 color="darkorange",
                 shape=21,
      ) +
#       geom_text(data=schools,
#                 aes_string(x="lon",
#                     y="lat",
#                     label="schoolname"
#                 ),
#                 color="forestgreen",
#                 size=text_size,
#                 hjust=hjust_adj,
#                 vjust=vjust_adj
#       ) +
      scale_fill_manual(values = c("gray", "forestgreen"))
  } else {
    neighborhood_map <- neighborhood_map +
      scale_fill_manual(values = c("gray", "forestgreen"))
  }
  neighborhood_map <- neighborhood_map +
    coord_map(projection="mercator",
              xlim=c(attr(gg_map, "bb")$ll.lon,
                     attr(gg_map, "bb")$ur.lon),
              ylim=c(attr(gg_map, "bb")$ll.lat,
                     attr(gg_map, "bb")$ur.lat)) +
    theme(legend.direction="horizontal")

  if (show_1s) {
    ones<-sqrp %>%
      filter(community_area %in% neighborhood_name,
             rating %in% c("1", "1+"))

    neighborhood_map <- neighborhood_map +
      geom_point(data=ones,
                 aes(x=lon,
                     y=lat,
                     alpha = total_sqrp_pionts),
                 color="forestgreen")
  }



  neighborhood_map

}
