require(sqrpr)
require(RJDBC)
require(dplyr)
#  get MAP data ####

setwd("~/Dropbox (KIPP Chicago Schools)/Data Analysis/SQRP/")
silo <- as.data.frame(read.dcf('config//silo_dw.dcf'))


drvr <- JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver',
             '~/Dropbox (KIPP Chicago Schools)/JDBC Drivers/sqljdbc_4.0/enu//sqljdbc4.jar')

conn <- dbConnect(drvr,
                  databaseName = as.character(silo$dbname),
                  url = as.character(silo$url),
                  user = as.character(silo$user),
                  password = as.character(silo$password)
)


# get viewAllAssessments ####
qry <- "SELECT * FROM NWEA..MAP$comprehensive#cps_included WHERE GrowthMeasureYN='TRUE'"
map_all_silo <- dbGetQuery(conn, qry)

# get AccomodationAssignments ####
qry <- "SELECT * FROM NWEA..AccommodationAssignment"
accomodations<-dbGetQuery(conn, qry)

names(accomodations) <- tolower(names(accomodations))


map_all_silo<-map_all_silo %>%
  mutate(SchoolName=ifelse(grepl("Ascend", SchoolName),
                           "KIPP Ascend College Prep",SchoolName
  ),
  Grade=ifelse(Grade=="K", 0, as.integer(Grade)),
  MeasurementScale=ifelse(grepl("General", MeasurementScale),
                          "General Science",
                          MeasurementScale)
  ) %>%
  filter(MeasurementScale %in% c("Reading", "Mathematics"))

names(map_all_silo) <- tolower(names(map_all_silo))

# Accomodationrogram Assignments ####
sped<-read.csv("data/sped_S15.csv")

sped15 <- sped %>% mutate(sped = "TRUE")


# sped15<-sped %>% filter(termname=="Spring 2014-2015") %>%
#   group_by(studentid) %>%
#   summarize(N=n()) %>%
#   mutate(sped="TRUE", studentid=as.numeric(studentid)) %>% select(-N)


#subset to group ####
map_f14s15<-map_all_silo %>%
  filter(termname %in% c("Fall 2014-2015", "Spring 2014-2015")) %>%
  left_join(sped15, by="studentid") %>%
  mutate(sped=ifelse(is.na(sped), "FALSE", sped))

map_f14<-map_all_silo %>%
  filter(termname %in% c("Fall 2014-2015")) %>%
  select(studentid, measurementscale, testritscore, grade)

map_s15<-map_all_silo %>%
  filter(termname %in% c("Spring 2014-2015"))



#calculate growth
kcs_growth<-school_growth_percentile(map_f14s15, fall_equate_scores = TRUE)

#calculate_attainment
kcs_attain<-school_attainment_percentile(map_s15)


#calculate growth priority groups ####
#AA
kcs_growth_aa<-priority_group(kcs_growth,
                              group_column = "studentethnicgroup",
                              group_id = "Black or African American",
                              fall_equate_scores = TRUE
                              )

#IEP
kcs_growth_dl<-priority_group(kcs_growth,
                              group_column = "sped",
                              group_id = "TRUE",
                              fall_equate_scores = TRUE
)

# calculate %me growth by school ####
# to do: move this to sqrpr
kcs_pct_me<-calc_pct_me(kcs_growth)


# KACP ##

sqrp_kacp<-sqrp_level("Ascend",
                           kcs_growth,
                           kcs_attain,
                           kcs_growth_aa,
                           kcs_growth_dl,
                           kcs_pct_me,
                           ada=.95,
                           mvms="WO",
                           dqi=.99
                           ) %>% mutate(school="KACP")

# KCCP ##

sqrp_kccp<-sqrp_level("Create",
                           kcs_growth,
                           kcs_attain,
                           kcs_growth_aa,
                           kcs_growth_dl,
                           kcs_pct_me,
                           ada=.95,
                           mvms="O",
                           dqi=.997
                      )%>% mutate(school="KCCP")

# KBCP ##

sqrp_kbcp<-sqrp_level("Bloom",
                           kcs_growth,
                           kcs_attain,
                           kcs_growth_aa,
                           kcs_growth_dl,
                           kcs_pct_me,
                           ada=.9,
                           mvms="WO",
                           dqi=.98
                      ) %>% mutate(school="KBCP")

rbind_list(sqrp_kacp, sqrp_kccp, sqrp_kbcp)


# suppose higher attendance #####

# KACP ##

sqrp_kacp<-sqrp_level("Ascend",
                      kcs_growth,
                      kcs_attain,
                      kcs_growth_aa,
                      kcs_growth_dl,
                      kcs_pct_me,
                      ada=.96,
                      mvms="WO",
                      dqi=.99
) %>% mutate(school="KACP")

# KCCP ##

sqrp_kccp<-sqrp_level("Create",
                      kcs_growth,
                      kcs_attain,
                      kcs_growth_aa,
                      kcs_growth_dl,
                      kcs_pct_me,
                      ada=.95,
                      mvms="O",
                      dqi=.997
)%>% mutate(school="KCCP")

# KBCP ##

sqrp_kbcp<-sqrp_level("Bloom",
                      kcs_growth,
                      kcs_attain,
                      kcs_growth_aa,
                      kcs_growth_dl,
                      kcs_pct_me,
                      ada=.95,
                      mvms="WO",
                      dqi=.99
) %>% mutate(school="KBCP")

rbind_list(sqrp_kacp, sqrp_kccp, sqrp_kbcp)

# 2013-14 #####

# sped
 sped14<-accomodations %>% filter(termname=="Spring 2013-2014") %>%
   group_by(studentid) %>%
   summarize(N=n()) %>%
   mutate(sped="TRUE", studentid=as.numeric(studentid)) %>% select(-N)


#subset to group ####
map_f13s14<-map_all_silo %>%
  filter(termname %in% c("Fall 2013-2014", "Spring 2013-2014")) %>%
  left_join(sped14, by="studentid") %>%
  mutate(sped=ifelse(is.na(sped), "FALSE", sped))

map_f13<-map_all_silo %>%
  filter(termname %in% c("Fall 2013-2014")) %>%
  select(studentid, measurementscale, testritscore, grade)

map_s14<-map_all_silo %>%
  filter(termname %in% c("Spring 2013-2014"))



#calculate growth
kcs_growth<-school_growth_percentile(map_f13s14, fall_equate_scores = TRUE)

#calculate_attainment
kcs_attain<-school_attainment_percentile(map_s14)


#calculate growth priority groups ####
#AA
kcs_growth_aa<-priority_group(kcs_growth,
                              group_column = "studentethnicgroup",
                              group_id = "Black or African American",
                              fall_equate_scores = TRUE
)

#IEP
kcs_growth_dl<-priority_group(kcs_growth,
                              group_column = "sped",
                              group_id = "TRUE",
                              fall_equate_scores = TRUE
)

# calculate %me growth by school ####
# to do: move this to sqrpr

kcs_pct_me<-calc_pct_me(kcs_growth)



# KACP ##

sqrp_kacp_1314<-sqrp_level("Ascend",
                      kcs_growth,
                      kcs_attain,
                      kcs_growth_aa,
                      kcs_growth_dl,
                      kcs_pct_me,
                      ada=.948,
                      mvms="WO",
                      dqi=.997
) %>% mutate(school="KACP")

# KCCP ##

sqrp_kccp_1314<-sqrp_level("Create",
                      kcs_growth,
                      kcs_attain,
                      kcs_growth_aa,
                      kcs_growth_dl,
                      kcs_pct_me,
                      ada=.951,
                      mvms="WO",
                      dqi=.997
)%>% mutate(school="KCCP")

# KBCP ##

sqrp_kbcp_1314<-sqrp_level("Bloom",
                      kcs_growth,
                      kcs_attain,
                      kcs_growth_aa,
                      kcs_growth_dl,
                      kcs_pct_me,
                      ada=.944,
                      dqi=.97
) %>% mutate(school="KBCP")

rbind_list(sqrp_kacp_1314, sqrp_kccp_1314, sqrp_kbcp_1314)


# 2012-13 #####

# sped
sped14<-accomodations %>% filter(termname=="Fall 2012-2013") %>%
  group_by(studentid) %>%
  summarize(N=n()) %>%
  mutate(sped="TRUE", studentid=as.numeric(studentid)) %>% select(-N)


#subset to group ####
map_f12s13<-map_all_silo %>%
  filter(termname %in% c("Fall 2012-2013", "Spring 2012-2013")) %>%
  left_join(sped14, by="studentid") %>%
  mutate(sped=ifelse(is.na(sped), "FALSE", sped),
         studentethnicgroup = ifelse(studentethnicgroup %in% c(3,4), "Black or African American", "Other"))

map_f12<-map_all_silo %>%
  filter(termname %in% c("Fall 2012-2013")) %>%
  select(studentid, measurementscale, testritscore, grade)

map_s13<-map_all_silo %>%
  filter(termname %in% c("Spring 2012-2013"))



#calculate growth
kcs_growth<-school_growth_percentile(map_f12s13, fall_equate_scores = TRUE)

#calculate_attainment
kcs_attain<-school_attainment_percentile(map_s13)


#calculate growth priority groups ####
#AA
kcs_growth_aa<-priority_group(kcs_growth,
                              group_column = "studentethnicgroup",
                              group_id = "Other",
                              fall_equate_scores = TRUE
)

#IEP
kcs_growth_dl<-priority_group(kcs_growth,
                              group_column = "sped",
                              group_id = "TRUE",
                              fall_equate_scores = TRUE
)

# calculate %me growth by school ####
# to do: move this to sqrpr

kcs_pct_me<-calc_pct_me(kcs_growth)



# KACP ##

sqrp_kacp_1213<-sqrp_level("Ascend",
                           kcs_growth,
                           kcs_attain,
                           kcs_growth_aa,
                           kcs_growth_dl,
                           kcs_pct_me,
                           ada=.945,
                           mvms="O",
                           dqi=.997
) %>% mutate(school="KACP")

# KCCP ##

sqrp_kccp_1213<-sqrp_level("Create",
                           kcs_growth,
                           kcs_attain,
                           kcs_growth_aa,
                           kcs_growth_dl,
                           kcs_pct_me,
                           ada=.956,
                           dqi=.997
)%>% mutate(school="KCCP")


rbind_list(sqrp_kacp_1213, sqrp_kccp_1213)
