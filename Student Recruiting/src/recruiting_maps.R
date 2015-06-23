setwd("~/Dropbox (KIPP Chicago Schools)/Data Analysis/Student Recruiting")

require(ProjectTemplate)


load.project()



# austin ####

austin<-get_map(location = "  4818 W Ohio St, Chicago, IL 60644",
            zoom=14,
            maptype = 'toner-lite',
            source = "stamen")

austin_map_base <- ggmap(austin,
                         base_layer = ggplot(data=municipalities.df,
                                             aes(x=long, y=lat)
                                             ),
                         extent = "normal",
                         maprange = FALSE
                         )


addr_austin <- combined_addresses %>%
  filter(community_area %in% c("AUSTIN", "WEST GARFIELD PARK"))



austin_map<-austin_map_base +
  #geom_density2d(data=addr_austin %>% filter(type=="Postcard"),
  #               aes(x=long, y=lat)
  #               ) +
  stat_density2d(data=addr_austin %>% filter(type=="Postcard"),
                 aes(x=long,
                     y=lat,
                     fill=..level..,
                     alpha=..level..),
                 size=0.01,
                 bins=32,
                 geom="polygon"
                 ) +
  geom_point(data=addr_austin %>% filter(type!="Postcard"),
             aes(x=long,
                 y=lat
             ),
             size=3,
             alpha=1
             )  +
  #geom_text(data=addr_austin %>% filter(type!="Postcard"),
  #           aes(x=jitter(long),
  #               y=jitter(lat),
  #               label=paste(first, last)
  #           ),
  #           size=4,
  #          hjust=.5,
  #          vjust=1
  #)  +
  scale_fill_gradient("Population Density",low="green", high="red") +
  scale_alpha(range=c(0,0.1), guide=FALSE) +
  coord_map(projection = "mercator",
            xlim=c(attr(austin, "bb")$ll.lon,
                   attr(austin, "bb")$ur.lon),
            ylim=c(attr(austin, "bb")$ll.lat,
                   attr(austin, "bb")$ur.lat)
            )



austin_map

# adding recruiting zones ####
kccp_rz1<-readOGR("kmls/kccp_rz1.kml", "kccp_rz1.kml") %>% fortify %>% mutate(rz="1")
kccp_rz2<-readOGR("kmls/kccp_rz2.kml", "kccp_rz2.kml") %>% fortify %>% mutate(rz="2")
kccp_rz3<-readOGR("kmls/kccp_rz3.kml", "kccp_rz3.kml") %>% fortify %>% mutate(rz="3")
kccp_rz4<-readOGR("kmls/kccp_rz4.kml", "kccp_rz4.kml") %>% fortify %>% mutate(rz="4")

kccp_rzs<-rbind_list(kccp_rz1, kccp_rz2, kccp_rz3, kccp_rz4)



austin_map +
  geom_polygon(data=kccp_rzs,
               aes(x=long,
                   y=lat,
                   group=rz),
               color="orange",
               fill="orange",
               size=3,
               alpha=.05)


# Englewood ####

englewood<-get_map(location = "  S HALSTED ST AND  W 57TH ST, Chicago, IL 60644",
                zoom=14,
                maptype = 'toner-lite',
                source = "stamen")

englwood_map_base <- ggmap(englewood,
                         base_layer = ggplot(data=municipalities.df,
                                             aes(x=long, y=lat)
                         ),
                         extent = "normal",
                         maprange = FALSE
)


addr_englwood <- combined_addresses %>%
  filter(community_area %in% c("ENGLEWOOD", "WEST ENGLEWOOD", "NEW CITY"))



englewood_map<-englwood_map_base +
  #geom_density2d(data=addr_austin %>% filter(type=="Postcard"),
  #               aes(x=long, y=lat)
  #               ) +
  stat_density2d(data=addr_englwood %>% filter(type=="Postcard"),
                 aes(x=long,
                     y=lat,
                     fill=..level..,
                     alpha=..level..),
                 size=0.005,
                 bins=50,
                 geom="polygon"
  ) +
  geom_point(data=addr_englwood %>% filter(type=="KIPPster"),
             aes(x=long,
                 y=lat
             ),
             size=3,
             alpha=1
  )  +
  #geom_text(data=addr_austin %>% filter(type!="Postcard"),
  #           aes(x=jitter(long),
  #               y=jitter(lat),
  #               label=paste(first, last)
  #           ),
  #           size=4,
  #          hjust=.5,
  #          vjust=1
  #)  +
  scale_fill_gradient("Population Density",low="green", high="red") +
  scale_alpha(range=c(0,0.1), guide=FALSE) +
  coord_map(projection = "mercator",
            xlim=c(attr(englewood, "bb")$ll.lon,
                   attr(englewood, "bb")$ur.lon),
            ylim=c(attr(englewood, "bb")$ll.lat,
                   attr(englewood, "bb")$ur.lat)
  )



englewood_map

# adding recruiting zones ####
kbcp_rz1_sp<-readOGR("kmls/kbcp_rz1.kml", "kbcp_rz1.kml") %>%
  spChFIDs("kbcp_rz1")

kbcp_rz2_sp<-readOGR("kmls/kbcp_rz2.kml", "kbcp_rz2.kml") %>%
  spChFIDs("kbcp_rz2")

kbcp_rz3_sp<-readOGR("kmls/kbcp_rz3.kml", "kbcp_rz3.kml") %>%
  spChFIDs("kbcp_rz3")

kbcp_rz4_sp<-readOGR("kmls/kbcp_rz4.kml", "kbcp_rz4.kml") %>%
  spChFIDs("kbcp_rz4")

kbcp_rz5_sp<-readOGR("kmls/kbcp_rz5.kml", "kbcp_rz5.kml") %>%
  spChFIDs("kbcp_rz5")


kbcp_rzs_sp<-rbind(kbcp_rz1_sp,
                   kbcp_rz2_sp,
                   kbcp_rz3_sp,
                   kbcp_rz4_sp,
                   kbcp_rz5_sp)

kbcp_rzs_sp <- spTransform(kbcp_rzs_sp, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


#proj4string(kbcp_rzs_sp)<-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


kbcp_rzs <- fortify(kbcp_rzs_sp)

# kbcp_rz1 <- kbcp_rz1_sp %>% fortify %>% mutate(rz="1")
# kbcp_rz2 <- kbcp_rz2_sp %>% fortify %>% mutate(rz="2")
# kbcp_rz3 <- kbcp_rz3_sp %>% fortify %>% mutate(rz="3")
# kbcp_rz4 <- kbcp_rz4_sp %>% fortify %>% mutate(rz="4")
# kbcp_rz5 <- kbcp_rz5_sp %>% fortify %>% mutate(rz="5")

#kbcp_rzs<-rbind_list(kbcp_rz1, kbcp_rz2, kbcp_rz3, kbcp_rz4, kbcp_rz5)

rz_names<-over(combined_sp, kbcp_rzs_sp)

combined_addresses$rz <- rz_names$Name


englwood_rz_addr <- combined_addresses %>%
  filter(grepl("kbcp", rz))

englewood_map_2 <- englwood_map_base +
#   geom_density2d(data=englwood_rz_addr,
#                 aes(x=long, y=lat),
#                 bins=10
#                 ) +
  stat_density2d(data=englwood_rz_addr,
                 aes(x=long,
                     y=lat,
                     fill=..level..,
                     alpha=..level..),
                 size=.005,
                 bins=35,
                 geom="polygon"
  ) +
  geom_point(data=englwood_rz_addr,
             aes(x=long,
                 y=lat,
                 color=type
             ),
             size=3,
             alpha=1

  )  +
  geom_polygon(data=kbcp_rzs,
               aes(x=long,
                   y=lat,
                   group=id),
               color="orange",
               fill="orange",
               size=3,
               alpha=.05) +
  #geom_text(data=addr_austin %>% filter(type!="Postcard"),
  #           aes(x=jitter(long),
  #               y=jitter(lat),
  #               label=paste(first, last)
  #           ),
  #           size=4,
  #          hjust=.5,
  #          vjust=1
  #)  +
  scale_fill_gradient("Population Density",low="green", high="red") +
  scale_alpha(range=c(0,0.1), guide=FALSE) +
  scale_color_manual("student", values=c("darkblue", "darkgreen", "gray")) +
  coord_map(projection = "mercator",
            xlim=c(attr(englewood, "bb")$ll.lon,
                   attr(englewood, "bb")$ur.lon),
            ylim=c(attr(englewood, "bb")$ll.lat,
                   attr(englewood, "bb")$ur.lat)
  ) +
  guides(color =  guide_legend(override.aes=list(fill=NA,
                                                 linetype=0))) +
  theme(legend.position="bottom")

englewood_map_2

cairo_pdf('graphs/kbcp_recruiting_map.pdf', height = 11, width=8.5)

englewood_map_2 +ggtitle("Englewood Recruitment Zones\nwith known addresses & student age density")

dev.off()

addr_kippsters_ordered<-englwood_rz_addr %>%
  filter(type!="Posstcard") %>%
  arrange(long, desc(lat)) %>%
  mutate(nu=row_number())

top_map<-englewood_map +
  geom_polygon(data=kbcp_rzs,
               aes(x=long,
                   y=lat,
                   group=id),
               color="orange",
               fill="orange",
               size=3,
               alpha=.05) +
  geom_text(data=addr_kippsters_ordered,
            aes(x=long, y=lat, label=nu), color="black", vjust=1.3)

require(gridExtra)


g_tbl<-addr_kippsters_ordered %>%
  mutate(Notes=NA) %>%
  select("ID" =nu,
          "First Name" = first,
         "Last Name" = last,
         "Grade" = grade,
         "Address" = address,
         Notes
         ) %>%
  head(15) %>%
tableGrob


grid.arrange(top_map, g_tbl, nrow=2)



