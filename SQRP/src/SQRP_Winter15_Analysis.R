#  get MAP data ####

setwd("~/Dropbox (KIPP Chicago Schools)/Data Analysis/SQRP/")
silo<-as.data.frame(read.dcf('config//silo_dw.dcf'))


drvr <- JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver',
             '~/Dropbox (KIPP Chicago Schools)/JDBC Drivers/sqljdbc_4.0/enu//sqljdbc4.jar')

conn <- dbConnect(drvr,
                  databaseName=as.character(silo$dbname),
                  url=as.character(silo$url),
                  user=as.character(silo$user),
                  password=as.character(silo$password)
)


# get viewAllAssessments ####
qry<-"SELECT * FROM NWEA..MAP$comprehensive#cps_included WHERE GrowthMeasureYN='TRUE'"
map_all_silo<-dbGetQuery(conn, qry)

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

# Program Assignments ####
sped<-dbGetQuery(conn, "SELECT *  FROM [NWEA].[dbo].[AccommodationAssignment]")

names(sped)<-tolower(names(sped))


sped14<-sped %>% filter(termname=="Spring 2013-2014") %>%
  group_by(studentid) %>%
  summarize(N=n()) %>%
  mutate(sped="TRUE", studentid=as.numeric(studentid)) %>% select(-N)


#subset to group ####
map_s13s14<-map_all_silo %>%
  filter(termname %in% c("Spring 2012-2013", "Spring 2013-2014")) %>%
  left_join(sped14, by="studentid") %>%
  mutate(sped=ifelse(is.na(sped), "FALSE", sped))

map_f14<-map_all_silo %>%
  filter(termname %in% c("Fall 2013-2014")) %>%
  select(studentid, measurementscale, testritscore, grade)

map_s14<-map_all_silo %>%
  filter(termname %in% c("Spring 2013-2014"))



#calculate growth
kcs_growth<-school_growth_percentile(map_s13s14, fall_equate_scores = map_f14)

#calculate_attainment
kcs_attain<-school_attainment_percentile(map_s14)


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





# KACP ##

sqrp_kacp<-sqrp_level("Ascend", 
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

sqrp_kccp<-sqrp_level("Create", 
                           kcs_growth, 
                           kcs_attain, 
                           kcs_growth_aa, 
                           kcs_growth_dl, 
                           kcs_pct_me, 
                           ada=.951,
                           mvms="WO",
                           dqi=.99
                      )%>% mutate(school="KCCP")

# KBCP ##

sqrp_kbcp<-sqrp_level("Bloom", 
                           kcs_growth, 
                           kcs_attain, 
                           kcs_growth_aa, 
                           kcs_growth_dl, 
                           kcs_pct_me, 
                           ada=.944,
                           dqi=.97
                      ) %>% mutate(school="KBCP")

rbind_list(sqrp_kacp, sqrp_kccp, sqrp_kbcp)

# lots o' Sims ####
set.seed(1945)

# KACP ####
map_s14_sped<-map_s14 %>% left_join(sped14, by="studentid")

map_f14_complete <- map_all_silo %>% 
  dplyr::filter(termname=="Fall 2014-2015",
                measurementscale %in% c("Reading", "Mathematics"))

sim_kacp_94_10<-simulate_sqrp(spring_pretest_cdf = map_s14_sped, 
                         fall_equate_cdf = map_f14_complete,
                         school = "Ascend",
                         ada=.94,
                         mvms="WO",
                         dqi=.997, 
                         pct_cr=.1,                 
                         n_sims=500)

do_all_sims <- function(school="Ascend", 
                        ada=c(.94,.95, .96),
                        mvms="WO",
                        dqi=.997,
                        pct_cr=c(.1,.25,.5,.75, .9),
                        n_sims=30){
  out<-list()  
  for(i in ada){
    for(j in pct_cr){
      suffix<-paste0("sim_",i*100,"_", j*100)
      
    out[[suffix]] <- simulate_sqrp(spring_pretest_cdf = map_s14_sped, 
                                   fall_equate_cdf = map_f14_complete,
                                   school = school,
                                   ada=i,
                                   mvms=mvms,
                                   dqi=dqi, 
                                   pct_cr=j,                 
                                   n_sims=n_sims) %>% 
      mutate(sim=suffix)
      
    }
  }
  
  out_df<-dplyr::rbind_all(out)
  
  out_summary<-out_df %>% 
    group_by(sim) %>%
    summarize(avg_points = mean(points), 
              sd_points=sd(points), 
              n_1=sum(level=="1"), 
              pct_1=n_1/n(),
              n_1_plus=sum(level=="1+"), 
              pct_1_plus=n_1_plus/n())
  
  out_list<-list(sims=out_df,
                 sim_summary=out_summary)
  
  out_list
    
}

# KACP for real ####

sims_KACP<-do_all_sims(school="Ascend", 
                      mvms="WO",
                      dqi=.99,
                      n_sims=100)

sims_KACP$sim_summary %>% 
  tidyr::extract(sim, 
                 c("ada", "pct_cr"), 
                 regex="sim_([[:digit:]]+)_([[:digit:]]+)") %>%
  select(ada, pct_cr, avg_points) %>%
  tidyr::spread(ada, avg_points)

sims_KACP$sim_summary %>% 
  tidyr::extract(sim, 
                 c("ada", "pct_cr"), 
                 regex="sim_([[:digit:]]+)_([[:digit:]]+)") %>%
  select(ada, pct_cr, avg_points) %>%
  tidyr::spread(ada, avg_points)




sims_KACP$sim_summary %>% 
  tidyr::extract(sim, 
                 c("ada", "pct_cr"), 
                 regex="sim_([[:digit:]]+)_([[:digit:]]+)") %>%
  mutate(point_range=paste(round(avg_points-sd_points,1), 
                           round(avg_points+sd_points,1),
                           sep="-")) %>%
  select(ada, pct_cr, point_range) %>%
  tidyr::spread(ada, point_range)

sims_KACP$sim_summary %>% 
  tidyr::extract(sim, 
                 c("ada", "pct_cr"), 
                 regex="sim_([[:digit:]]+)_([[:digit:]]+)") %>%
  select(ada, pct_cr, pct_1) %>%
  tidyr::spread(ada, pct_1)

sims_KACP$sim_summary %>% 
  tidyr::extract(sim, 
                 c("ada", "pct_cr"), 
                 regex="sim_([[:digit:]]+)_([[:digit:]]+)") %>%
  select(ada, pct_cr, pct_1_plus) %>%
  tidyr::spread(ada, pct_1_plus)

sims_KACP$sim_summary %>% 
  tidyr::extract(sim, 
                 c("ada", "pct_cr"), 
                 regex="sim_([[:digit:]]+)_([[:digit:]]+)") %>%
  mutate(prob_1_1plus=pct_1_plus + pct_1) %>%
  select(ada, pct_cr, prob_1_1plus) %>%
  tidyr::spread(ada, prob_1_1plus)

# KCCP for real ####

sims_KCCP<-do_all_sims(school="Create", 
                       mvms="WO",
                       dqi=.99,
                       n_sims=100)


sims_KCCP$sim_summary %>% 
  tidyr::extract(sim, 
                 c("ada", "pct_cr"), 
                 regex="sim_([[:digit:]]+)_([[:digit:]]+)") %>%
  mutate(point_range=paste(round(avg_points-sd_points,1), 
                           round(avg_points+sd_points,1),
                           sep="-")) %>%
  select(ada, pct_cr, point_range) %>%
  tidyr::spread(ada, point_range)


sims_KCCP$sim_summary %>% 
  tidyr::extract(sim, 
                 c("ada", "pct_cr"), 
                 regex="sim_([[:digit:]]+)_([[:digit:]]+)") %>%
  select(ada, pct_cr, pct_1) %>%
  tidyr::spread(ada, pct_1)

sims_KCCP$sim_summary %>% 
  tidyr::extract(sim, 
                 c("ada", "pct_cr"), 
                 regex="sim_([[:digit:]]+)_([[:digit:]]+)") %>%
  select(ada, pct_cr, pct_1_plus) %>%
  tidyr::spread(ada, pct_1_plus)

sims_KCCP$sim_summary %>% 
  tidyr::extract(sim, 
                 c("ada", "pct_cr"), 
                 regex="sim_([[:digit:]]+)_([[:digit:]]+)") %>%
  mutate(prob_1_1plus=pct_1_plus + pct_1) %>%
  select(ada, pct_cr, prob_1_1plus) %>%
  tidyr::spread(ada, prob_1_1plus)


# KBCP ####
sims_KBCP<-do_all_sims(school="Bloom", 
                       mvms="WO",
                       ada=c(.93),
                       dqi=.99,
                       n_sims=100)



sims_KBCP$sim_summary %>% 
  tidyr::extract(sim, 
                 c("ada", "pct_cr"), 
                 regex="sim_([[:digit:]]+)_([[:digit:]]+)") %>%
  mutate(point_range=paste(round(avg_points-sd_points,1), 
                           round(avg_points+sd_points,1),
                           sep="-")) %>%
  select(ada, pct_cr, point_range) %>%
  tidyr::spread(ada, point_range)

sims_KBCP$sim_summary %>% 
  tidyr::extract(sim, 
                 c("ada", "pct_cr"), 
                 regex="sim_([[:digit:]]+)_([[:digit:]]+)") %>%
  select(ada, pct_cr, pct_1) %>%
  tidyr::spread(ada, pct_1)

sims_KBCP$sim_summary %>% 
  tidyr::extract(sim, 
                 c("ada", "pct_cr"), 
                 regex="sim_([[:digit:]]+)_([[:digit:]]+)") %>%
  select(ada, pct_cr, pct_1_plus) %>%
  tidyr::spread(ada, pct_1_plus)

sims_KBCP$sim_summary %>% 
  tidyr::extract(sim, 
                 c("ada", "pct_cr"), 
                 regex="sim_([[:digit:]]+)_([[:digit:]]+)") %>%
  mutate(prob_1_1plus=pct_1_plus + pct_1) %>%
  select(ada, pct_cr, prob_1_1plus) %>%
  tidyr::spread(ada, prob_1_1plus)


sims_KBCP$sims %>% group_by(sim) %>%
  summarize(n_2plus=sum(level=="2+"), pct_2plus=n_2plus/n())  %>% 
  tidyr::extract(sim, 
                 c("ada", "pct_cr"), 
                 regex="sim_([[:digit:]]+)_([[:digit:]]+)") %>%
  select(ada, pct_cr, pct_2plus) %>%
  tidyr::spread(ada, pct_2plus)

  
