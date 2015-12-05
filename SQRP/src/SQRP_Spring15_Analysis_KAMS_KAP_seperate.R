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
  mutate(
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
map_s14s15<-map_all_silo %>%
  filter(termname %in% c("Spring 2013-2014", "Spring 2014-2015")) %>%
  left_join(sped15, by="studentid") %>%
  mutate(sped=ifelse(is.na(sped), "FALSE", sped))

map_f14<-map_all_silo %>%
  filter(termname %in% c("Fall 2014-2015")) %>%
  select(studentid, measurementscale, testritscore, grade)

map_s15<-map_all_silo %>%
  filter(termname %in% c("Spring 2014-2015"))



#calculate growth
kcs_growth<-school_growth_percentile(map_s14s15, fall_equate_scores = map_f14)

#calculate_attainment
kcs_attain<-school_attainment_percentile(map_s15)


#calculate growth priority groups ####
#AA
kcs_growth_aa<-priority_group(kcs_growth,
                              group_column = "studentethnicgroup",
                              group_id = "Black or African American",
                              fall_equate_scores = map_f14
                              )

#IEP
kcs_growth_dl<-priority_group(kcs_growth,
                              group_column = "sped",
                              group_id = "TRUE",
                              fall_equate_scores = map_f14
)

# calculate %me growth by school ####
# to do: move this to sqrpr
kcs_pct_me<-calc_pct_me(kcs_growth)





# KAP ##

sqrp_kap<-sqrp_level("Ascend Primary",
                           kcs_growth,
                           kcs_attain,
                           kcs_growth_aa,
                           kcs_growth_dl,
                           kcs_pct_me,
                           ada=.95,
                           mvms="WO",
                           dqi=.99
                           ) %>% mutate(school="KAP")

# KAMS ##

kams<-sqrp_level("Ascend Middle",
                      kcs_growth,
                      kcs_attain,
                      kcs_growth_aa,
                      kcs_growth_dl,
                      kcs_pct_me,
                      ada=.95,
                      mvms="WO",
                      dqi=.99
) %>% mutate(school="KAMS")




# moving 5th from KAP to KAMS ####

map_2 <-map_all_silo %>%
  mutate(schoolname = ifelse(grepl("Ascend", schoolname) &
                               grade == 5,
                             "KIPP Ascend Middle School",
                             schoolname)
         )

#subset to group ####
map_s14s15_2<-map_2 %>%
  filter(termname %in% c("Spring 2013-2014", "Spring 2014-2015")) %>%
  left_join(sped15, by="studentid") %>%
  mutate(sped=ifelse(is.na(sped), "FALSE", sped))

map_f14_2<-map_2 %>%
  filter(termname %in% c("Fall 2014-2015")) %>%
  select(studentid, measurementscale, testritscore, grade)

map_s15_2<-map_2 %>%
  filter(termname %in% c("Spring 2014-2015"))



#calculate growth
kcs_growth_2<-school_growth_percentile(map_s14s15_2, fall_equate_scores = map_f14_2)

#calculate_attainment
kcs_attain_2<-school_attainment_percentile(map_s15_2)


#calculate growth priority groups ####
#AA
kcs_growth_aa_2<-priority_group(kcs_growth_2,
                              group_column = "studentethnicgroup",
                              group_id = "Black or African American",
                              fall_equate_scores = map_f14_2
)

#IEP
kcs_growth_dl_2<-priority_group(kcs_growth_2,
                              group_column = "sped",
                              group_id = "TRUE",
                              fall_equate_scores = map_f14_2
)

# calculate %me growth by school ####
# to do: move this to sqrpr
kcs_pct_me_2<-calc_pct_me(kcs_growth_2)


# KAP less 5th ##

sqrp_kap_2<-sqrp_level("Ascend Primary",
                     kcs_growth_2,
                     kcs_attain_2,
                     kcs_growth_aa_2,
                     kcs_growth_dl_2,
                     kcs_pct_me_2,
                     ada=.95,
                     mvms="WO",
                     dqi=.99
) %>% mutate(school="KAP")

# KAMS ##

sqrp_kams_2<-sqrp_level("Ascend Middle",
                      kcs_growth_2,
                      kcs_attain_2,
                      kcs_growth_aa_2,
                      kcs_growth_dl_2,
                      kcs_pct_me_2,
                      ada=.95,
                      mvms="WO",
                      dqi=.99
) %>% mutate(school="KAMS")




# Separateing 13-14 data ####
# map_all_silo_combinded <- map_all_silo %>%
#   mutate(schoolname = ifelse(grepl("Ascend", schoolname), "Ascend",))
# Accomodationrogram Assignments ####


sped14 <- accomodations %>%
  filter(termname == "Spring 2013-2014") %>%
  select(studentid) %>%
  group_by(studentid) %>%
  summarize(N_accom=n()) %>%
  mutate(sped = "TRUE",
         studentid = as.numeric(studentid))


# sped15<-sped %>% filter(termname=="Spring 2014-2015") %>%
#   group_by(studentid) %>%
#   summarize(N=n()) %>%
#   mutate(sped="TRUE", studentid=as.numeric(studentid)) %>% select(-N)


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
kcs_growth<-school_growth_percentile(map_f13s14, fall_equate_scores = map_f13)

#calculate_attainment
kcs_attain<-school_attainment_percentile(map_s14)


#calculate growth priority groups ####
#AA
kcs_growth_aa<-priority_group(kcs_growth,
                              group_column = "studentethnicgroup",
                              group_id = "Black or African American",
                              fall_equate_scores = map_f13
)

#IEP
kcs_growth_dl<-priority_group(kcs_growth,
                              group_column = "sped",
                              group_id = "TRUE",
                              fall_equate_scores = map_f13
)

# calculate %me growth by school ####
# to do: move this to sqrpr
kcs_pct_me<-calc_pct_me(kcs_growth)





# KAP ##

sqrp_kap<-sqrp_level("Ascend Primary",
                     kcs_growth,
                     kcs_attain,
                     kcs_growth_aa,
                     kcs_growth_dl,
                     kcs_pct_me,
                     ada=.95,
                     mvms="WO",
                     dqi=.99
) %>% mutate(school="KAP")

# KAMS ##

kams<-sqrp_level("Ascend Middle",
                 kcs_growth,
                 kcs_attain,
                 kcs_growth_aa,
                 kcs_growth_dl,
                 kcs_pct_me,
                 ada=.948,
                 mvms="WO",
                 dqi=.99
) %>% mutate(school="KAMS")



# moving 5th from KAMS to KAP ####

map_2 <-map_all_silo %>%
  mutate(schoolname = ifelse(grepl("Ascend", schoolname) &
                               grade == 5,
                             "KIPP Ascend Primary School",
                             schoolname)
  )

#subset to group ####
map_f13s14_2<-map_2 %>%
  filter(termname %in% c("Fall 2013-2014", "Spring 2013-2014")) %>%
  left_join(sped14, by="studentid") %>%
  mutate(sped=ifelse(is.na(sped), "FALSE", sped))

map_f13_2<-map_2 %>%
  filter(termname %in% c("Fall 2013-2014")) %>%
  select(studentid, measurementscale, testritscore, grade)

map_s14_2<-map_2 %>%
  filter(termname %in% c("Spring 2013-2014"))



#calculate growth
kcs_growth_2<-school_growth_percentile(map_f13s14_2, fall_equate_scores = map_f13_2)

#calculate_attainment
kcs_attain_2<-school_attainment_percentile(map_s14_2)


#calculate growth priority groups ####
#AA
kcs_growth_aa_2<-priority_group(kcs_growth_2,
                                group_column = "studentethnicgroup",
                                group_id = "Black or African American",
                                fall_equate_scores = map_f13_2
)

#IEP
kcs_growth_dl_2<-priority_group(kcs_growth_2,
                                group_column = "sped",
                                group_id = "TRUE",
                                fall_equate_scores = map_f13_2
)

# calculate %me growth by school ####
# to do: move this to sqrpr
kcs_pct_me_2<-calc_pct_me(kcs_growth_2)


# KAP less 5th ##

sqrp_kap_2<-sqrp_level("Ascend Primary",
                       kcs_growth_2,
                       kcs_attain_2,
                       kcs_growth_aa_2,
                       kcs_growth_dl_2,
                       kcs_pct_me_2,
                       ada=.95,
                       mvms="WO",
                       dqi=.99
) %>% mutate(school="KAP")

# KAMS ##

sqrp_kams_2<-sqrp_level("Ascend Middle",
                        kcs_growth_2,
                        kcs_attain_2,
                        kcs_growth_aa_2,
                        kcs_growth_dl_2,
                        kcs_pct_me_2,
                        ada=.95,
                        mvms="WO",
                        dqi=.99
) %>% mutate(school="KAMS")



