require(ProjectTemplate)
load.project()

map_all_silo<-map_all_silo %>% 
  mutate(SchoolName=ifelse(grepl("Ascend", SchoolName),
                           "KIPP Ascend College Prep",SchoolName
                           ),
         Grade=ifelse(Grade=="K", 0, as.integer(Grade)),
         MeasurementScale=ifelse(grepl("General", MeasurementScale), 
                                 "General Science",
                                 MeasurementScale)
         ) %>%
  filter(MeasurementScale %in% c("Reading", "Mathematics", "General Science"))


# unadjested schoorss ###
map_mv<-mapvizier(map_all_silo)

map_mv_summ<-summary(map_mv) %>% as.data.frame

# CPS Adjusted scores  ####

# first pull spring and fall events, match missing 
# pull original map data with seasons and years and what not
map_processed <- map_mv$mapData %>% 
  filter(MeasurementScale %in% c("Reading", "Mathematics", "General Science"))

# pull out fall, only need student ID, year, measurement scale, rit score
fall<-map_processed %>% 
  filter(Season=="Fall") %>%
  mutate(match_year=Year1,
         fall_rit=TestRITScore,
         Grade!=0) %>%
  select(StudentID, MeasurementScale, match_year, fall_grade=Grade, fall_rit)

# pull out spring, as above
spring<-map_processed %>% 
  filter(Season=="Spring") %>%
  mutate(match_year=Year2,
         spring_rit=TestRITScore) %>%
  select(StudentID, MeasurementScale, match_year, spring_rit)


# match fall adn spring, keeping all fall students
fall_spring<-left_join(fall,
                       spring,
                       by=c("StudentID", "MeasurementScale", "match_year")
                       ) %>%
  filter(is.na(spring_rit))

assert_that(all(is.na(fall_spring$spring_rit)))

#update spring scores for those that are missing
fall_spring_cps<-fall_spring %>%
  mutate(spring_rit=cps_equate(rit_score = fall_rit,
                               grade = fall_grade,
                               subject = MeasurementScale
                               ) %>% round
         ) %>%
  select(-fall_grade, -fall_rit)
  

assert_that(all(!is.na(fall_spring_cps$spring_rit)))

#pull only updated students from fall data, update term_name, season, and
# year fields.

map_cps_equated <- map_processed %>% 
  filter(Season=="Fall") %>%
  mutate(match_year=Year1) %>%
  inner_join(fall_spring_cps,              
            by=c("StudentID",
                  "MeasurementScale",
                   "match_year")) %>%
  mutate(Season="Spring", 
         Grade=Grade-1,
         Year1=Year1-1, 
         Year2=Year2-1,
         TermName=paste0(Season, " ", Year1,"-", Year2),
         SY=paste0(Year1, "-", Year2),
         TestRITScore=spring_rit,
         CPS_Equated=TRUE) %>%
  select(-match_year, -spring_rit)

assert_that(nrow(fall_spring_cps)==nrow(map_cps_equated))


 
#append to processed data.

map_combined<-rbind_list(map_processed, map_cps_equated) %>%
  mutate(ifelse(is.na(CPS_Equated), FALSE, CPS_Equated))

assert_that(nrow(map_combined)==nrow(map_processed) + nrow(map_cps_equated))

#  rerun mapvizier on combined data
# Reduce columns to initials silo data

sel_cols <- names(map_all_silo)

map_combined_2<-map_combined[,sel_cols] %>% as.data.frame

map_mv_equated <- mapvizier(map_combined_2)

map_mv_summ_equated <- summary(map_mv_equated) %>% as.data.frame

# Plots ####
# Longitudinal ####



grade<-3


school<-"KACP"
subject<-"General Science"


summary_plots(grade=grade, subject=subject, school = school) %>% print

summary_plots(grade=0, subject="Mathematics", school = "KACP") %>% print


# Student Percentile Plots ####
map_current<-map_mv$mapData %>% 
  filter(StudentID %in% current_ps_roster$StudentID) %>%
  group_by(StudentID, MeasurementScale) %>%
  mutate(CurrentGrade=max(Grade))



pdf(file = "graphs//test.pdf", width=10.75, heigh=9.25, onefile = TRUE)
student_npr_long_plot("KACP", 7, "Mathematics")
dev.off()






loop_data<-map_processed %>% filter(Season=="Fall") %>%
  select(SchoolInitials, Grade, MeasurementScale) %>%
  unique %>% 
  arrange(SchoolInitials, MeasurementScale, desc(Grade)) %>%
  select(school=SchoolInitials, subject=MeasurementScale, grade=Grade)

combine_plots <- function(school, subject, grade){
  page1 <- summary_plots(school=school, subject=subject, grade=grade)
  page2 <- student_npr_long_plot(school=school, subject=subject, grade=grade) %>%
    print
  
  #return
  list(page1, page2)
}

# all schools
sum_plots<-list()
#sum_plots<-apply(loop_data, 1, function(x) summary_plots(school = x[1], subject = x[2], grade=as.integer(x[3])))
sum_plots<-apply(loop_data, 1, function(x) combine_plots(school = x[1], subject = x[2], grade=as.integer(x[3])))

pdf(file = "graphs//Winter_two_pager.pdf", width=10.75, height=9.25, onefile = TRUE)
sum_plots
dev.off()



#KACP
sum_plots_KACP<-apply(loop_data %>% filter(school=="KACP"), 
                      1, 
                      function(x) combine_plots(school = x[1], subject = x[2], grade=as.integer(x[3])))

pdf(file = "graphs//Winter_two_pager_KACP.pdf", width=10.75, height=9.25, onefile = TRUE)
sum_plots_KACP
dev.off()


#KAP 
sum_plots_KAP<-apply(loop_data %>% filter(school=="KACP", grade<=5), 
                      1, function(x) combine_plots(school = x[1], subject = x[2], grade=as.integer(x[3])))

pdf(file = "graphs//Winter_two_pager_KAP.pdf", width=10.75, height=9.25, onefile = TRUE)
sum_plots_KAP
dev.off()


#KAMS
sum_plots_KAMS<-apply(loop_data %>% filter(school=="KACP", grade>5), 
                      1, function(x) combine_plots(school = x[1], subject = x[2], grade=as.integer(x[3])))
pdf(file = "graphs//Winter_two_pager_KAMS.pdf", width=10.75, height=9.25, onefile = TRUE)
sum_plots_KAMS
dev.off()

#KCCP
sum_plots_KCCP<-apply(loop_data %>% filter(school=="KCCP"), 
                      1, function(x) combine_plots(school = x[1], subject = x[2], grade=as.integer(x[3])))

pdf(file = "graphs//Winter_two_pager_KCCP.pdf", width=10.75, height=9.25, onefile = TRUE)
sum_plots_KCCP
dev.off()


#KBCP
sum_plots_KBCP<-apply(loop_data %>% filter(school=="KBCP"), 
                      1, function(x) combine_plots(school = x[1], subject = x[2], grade=as.integer(x[3])))

pdf(file = "graphs//Winter_two_pager_KBCP.pdf", width=10.75, height=9.25, onefile = TRUE)
sum_plots_KBCP
dev.off()







