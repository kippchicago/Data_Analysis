create_rfp_map <- function(gg_map,
                            .data,
                            neighborhood_name,
                            fill_column="level",
                            color_column = "level",
                            size_column=NA,
                            shape_column=NA,
                            text_size=4,
                            vjust_adj=0.1,
                            hjust_adj=0,
                            sqrp_cols = RColorBrewer::brewer.pal(n = 5, name = "BrBG"),
                            ...){
  
  
  # get colors setup
  
  sqrp_levels<-rev(c("1+", "1", "2+", "2", "3", "Not rated"))
  
  cols_df<-data.frame(level_cols=c("gray90", sqrp_cols),
                      level=factor(sqrp_levels, 
                                   levels=sqrp_levels, 
                                   ordered=TRUE
                                   )
                      )
  
  .data<-.data %>% 
    filter(community_area %in% neighborhood_name) %>%
    inner_join(cols_df, by="level")
  
  
  nn_caps<-toupper(neighborhood_name)
  
    
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
                 fill="gray70",
                 color='orange3',
                 size=2,
                 alpha=.8
    ) +
    geom_point(data=.data %>% filter(network!="CHARTER"), 
              # color="black",
               #size=3,
               alpha=.8,
               #shape=21,
               aes_string(x="lon",
                   y="lat",
                   #fill="level_cols",
                   color="level_cols",
                   size=size_column,
                   shape=shape_column
               )
    ) + 
    geom_point(data=.data  %>% filter(network=="CHARTER"), 
               # color="black",
               #size=3,
               alpha=.8,
               #shape=21,
               aes_string(x="lon",
                          y="lat",
                          fill="level_cols",
                          color="level_cols",
                          size=size_column,
                          shape=shape_column
               )
    ) + 
    geom_text(data=.data %>% filter(network!="CHARTER"),
              aes(x=lon, 
                  y=lat, 
                  label=INCS_School_Name
              ),
              color="black", 
              vjust=1,
              hjust=1,
              size=2
    ) + 
    geom_text(data=.data %>% filter(network=="CHARTER"),
              aes(x=lon, 
                  y=lat, 
                  label=INCS_School_Name
              ),
              color="hotpink", 
              vjust=0,
              hjust=0,
              size=2
    )
  
  
    neighborhood_map <- neighborhood_map +
      scale_fill_identity("SQRP Level",
      labels = as.character(cols_df$level), 
      breaks=cols_df$level_cols,
      guide="legend") +
    scale_color_identity("SQRP Levels",
                           labels = as.character(cols_df$level), 
                           breaks=cols_df$level_cols,
                           guide="legend") +
      scale_size("% residents NOT attending", range = c(3,8)) + 
      scale_shape("Adj. Utiliz. Status")
  
  
  
  neighborhood_map <- neighborhood_map +
    coord_map(projection="mercator", 
              xlim=c(attr(gg_map, "bb")$ll.lon, 
                     attr(gg_map, "bb")$ur.lon),
              ylim=c(attr(gg_map, "bb")$ll.lat,
                     attr(gg_map, "bb")$ur.lat)) +
    theme(legend.direction="horizontal")
  
  neighborhood_map
 
}
