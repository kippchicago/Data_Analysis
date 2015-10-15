# combine CPS data

sqrp<-sqrp.cps %>%
  left_join(cps.schools.addresses %>%
              mutate(schoolid=as.integer(CPS_ID)) %>%
              select(schoolid,lat=Lat, lon=Long),
            by="schoolid"
            ) %>%
  left_join(cps.schools.community.areas %>%
              mutate(schoolid=as.integer(schoolid)) %>%
              select(schoolid, community_area),
            by="schoolid"
            ) %>%
  mutate(rating=str_replace(sqrp_rating, "Level ", ""),
         rating=factor(rating,
                       levels=rev(c("1+", "1", "2+", "2", "3")),
                       ordered=TRUE
                       ),
         level=FALSE,
         level_1s = ifelse(grepl("1", as.character(rating)), as.character(rating),
                           "2+, 2, or 3"),
         level_1s = factor(level_1s, levels = rev(c("1+", "1", "2+, 2, or 3")),
                           ordered = TRUE)
         )


# geocode KCS locations
kcs_schools_location<-data.frame(schoolname=c("KIPP Ascend Primary",
                                            "KIPP Ascend Middle",
                                            "KIPP Create",
                                            "KIPP Bloom"),
                                 schoolintials=c("KAP", "KAMS", "KCCP", "KBCP"),
                                 address=c("1440 S Christiana 60623",
                                           "1616 S Avers Ave 60623",
                                           "4818 W Ohio St 60644",
                                           "5515 S Lowe Ave 60621"),
                                 rating=factor(c("1+", "1+", "1+", "1"),
                                               levels=rev(c("1+", "1", "2+", "2", "3")),
                                               ordered=TRUE)
                                 ) %>%
  mutate(level=rating %in% c("1", "1+"),
         level_1s = ifelse(grepl("1", as.character(rating)), as.character(rating),
                           "2+, 2, or 3"),
         level_1s = factor(level_1s, levels = rev(c("1+", "1", "2+, 2, or 3")),
                           ordered = TRUE)

         )


schools_geocoded<-geocode(kcs_schools_location$address)

kcs_schools_location %<>% mutate(lon=schools_geocoded$lon, lat=schools_geocoded$lat)
