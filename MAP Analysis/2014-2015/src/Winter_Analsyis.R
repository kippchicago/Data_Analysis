require(ProjectTemplate)
load.project()

map_all_silo<-map_all_silo %>% 
  mutate(SchoolName=ifelse(grepl("Ascend", SchoolName),
                           "KIPP Ascend College Prep",SchoolName
                           ),
         Grade=ifelse(Grade=="K", 0, as.integer(Grade))
         )i


# unadjested schoorss ###
map_mv<-mapvizier(map_all_silo)

map_mv_summ<-summary(map_mv) %>% as.data.frame

# CPS Adjusted scores  ####

# first pull spring and fall events, match missing 
# pull original map data with seasons and years and what not
map_processed <- map_mv$mapData %>% 
  filter(MeasurementScale %in% c("Reading", "Mathematics"))

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


summary_plots <- function(grade, subject,  school){
  map_fw<-map_mv_summ %>%
    filter(Grade==grade, 
           School==school,
           Subject==subject,
           GrowthSeason=="Fall - Winter",
           N>10)
  
  map_sw<-map_mv_summ %>%
    filter(Grade==grade, 
           School==school,
           Subject==subject,
           GrowthSeason=="Spring - Winter", 
           N>10)
  
  
  map_sw_equated<-map_mv_summ_equated %>%
    filter(Grade==grade, 
           School==school,
           Subject==subject,
           GrowthSeason=="Spring - Winter", 
           N>10) %>%
    mutate(GrowthSeason="Spring - Winter\n(CPS equated)")
  
  map_test<-rbind(map_sw, map_fw, map_sw_equated) %>%
    na.omit
  
  p<-long_summary_plot(map_test,
                       facets= GrowthSeason ~ .)
  
  
  # Becca Plots ####
  
  # vectorize Andrew's mapvisuals::grade_level_season
  gls<-Vectorize(grade_level_season)
  
  cy <- 2015+12-grade
  
  data_bp<-map_mv$mapData %>% 
    filter(CohortYear==cy,
           MeasurementScale==subject,
           SchoolInitials==school) %>% 
    mutate(GradeSeason=Grade+gls(Season))
  
  #  data_fake_national<-data.frame(GradeSeason=rep(2, times=8),
  #                                 MeasurementScale=c(rep("Mathematics", 4), 
  #                                                    rep("Reading", 4)),
  #                                 TestPercentile=rep(seq(24,100, by=25),2),
  #                                 CohortYear=rep(2021, 8),
  #                                 Year2=rep(2014,8),
  #                                 DistrictName="National"
  # #                            )
  
  
  data_fake_national<-data.frame(GradeSeason=rep(2, times=12),
                                 MeasurementScale=c(rep("Mathematics", 4), 
                                                    rep("Reading", 4),
                                                    rep("General Science", 4)),
                                 TestPercentile=rep(seq(24,100, by=25),3),
                                 CohortYear=rep(2021, 12),
                                 Year2=rep(2014,12),
                                 DistrictName="National Distribution"
  )
  
  data_bp2<-data_bp %>% select(GradeSeason,
                               MeasurementScale,
                               TestPercentile,
                               CohortYear,
                               Year2,
                               SchoolInitials)
  
  p_bp<-becca_plot(.data = as.data.frame(data_bp2), 
                   grade_level_season_column="GradeSeason",
                   school_name_column="SchoolInitials", 
                   cohort_name_column="CohortYear",
                   #academic_year_column="Year2",
                   measurement_scale_column="MeasurementScale", 
                   test_percentile_column="TestPercentile",
                   facets=". ~ MEASUREMENTSCALE", 
                   justify_widths=FALSE, 
                   #justify_min=4.2, 
                   #justify_max=5, 
                   first_and_spring_only=FALSE,
                   small_n_cutoff = 0.1
  )
  
  p_bp2<-p_bp + scale_fill_manual(values = c("#8D8685",  #1st 
                                             "#CFCCC1",  #2nd
                                             "#A7CFEE",  #3rd
                                             "#60A2D7"   #4th
  )
  ) +  
    xlab("") + 
    guides(fill=guide_legend(title="Quartiles")) +
    theme(axis.ticks=element_blank(),
          axis.text.x=element_text(size=8)
    )
  
  # Page one ####
  require(gridExtra)
  
  message(paste("Plotting", school, subject, grade))
  plots<-arrangeGrob(p + ggtitle("Historic Grade-level Growth"), 
               p_bp2 + ggtitle("Historic Cohort Attainment") , ncol=2)
  
  titles<-textGrob(label = paste("Winter MAP Results", 
                                 school, 
                                 grade,
                                 subject,
                                 sep=" | "))
  arrangeGrob(titles, plots, nrow=2, heights=c(unit(.1, "npc"),
                                                unit(.9, "npc")
                                                )
               )
}

grade<-7


school<-"KACP"
subject<-"Mathematics"

summary_plots(grade=grade, subject=subject, school = school)

summary_plots(grade=0, subject="Mathematics", school = "KACP")


# Student Percentile Plots ####
fsm<-Vectorize(fall_spring_me)
fssm <- Vectorize(fall_spring_sort_me)

map_current<-map_mv$mapData %>% 
  filter(StudentID %in% current_ps_roster$StudentID) %>%
  group_by(StudentID, MeasurementScale) %>%
  mutate(CurrentGrade=max(Grade))

student_npr_long_plot <- function(school, grade, subject){
  map_individual<-map_current %>% filter(SchoolInitials==school,
                                         CohortYear==2015+12-grade,
                                         MeasurementScale==subject) %>%
    mutate(gls=Grade+gls(fws_string = Season),
           Assessment=fsm(gls),
           Asses_sort=fssm(gls),
           Name=paste(StudentLastName, StudentFirstName, sep=", "))
  
  assessment_order<-map_individual %>% 
    ungroup %>% 
    select(Assessment, Asses_sort) %>% 
    unique %>% 
    mutate(Assessment=factor(Assessment, levels = Assessment))
  
  map_individual <- map_individual %>%
    mutate(Assessment=factor(Assessment, levels=assessment_order$Assessment))
  
  
  
  min_grade <- min(map_individual$gls) 
  max_grade <- max(map_individual$gls)
  assessments_breaks<-unique(map_individual$gls)
  
  p_indv<-ggplot(map_individual, aes(x=gls, y=TestPercentile)) +
    geom_line(aes(group=StudentID)) +
    geom_point(size=2) +
    annotate("rect", ymin=0, ymax=25, xmin=min_grade, xmax=max_grade, fill="red", alpha=.1) +
    annotate("rect", ymin=25, ymax=50, xmin=min_grade, xmax=max_grade, fill="gold", alpha=.1) +
    annotate("rect", ymin=50, ymax=75, xmin=min_grade, xmax=max_grade, fill="blue", alpha=.1) +
    annotate("rect", ymin=75, ymax=100, xmin=min_grade, xmax=max_grade, fill="green", alpha=.1) +
    stat_smooth(method="lm", se = FALSE) + 
    facet_wrap(~Name, ncol = 10) + 
    scale_x_continuous("Assessments", 
                       breaks=assessments_breaks,
                       labels=assessment_order$Assessment) +
    theme_minimal() + 
    theme(strip.text=element_text(size=8),
          axis.text.y=element_text(size=6),
          axis.text.x=element_text(size=4)  
    ) +
    ylab("National Percentile Rank") + 
    ggtitle(paste("Historical Percentile Ranks", school, subject, grade, sep=" | "))
  
  #return
  p_indv
  
}



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
  page2 <- student_npr_long_plot(school=school, subject=subject, grade=grade)
  
  #return
  list(page1, page2)
}


sum_plots<-list()
#sum_plots<-apply(loop_data, 1, function(x) summary_plots(school = x[1], subject = x[2], grade=as.integer(x[3])))
sum_plots<-apply(loop_data, 1, function(x) combine_plots(school = x[1], subject = x[2], grade=as.integer(x[3])))


pdf(file = "graphs//Winter_two_pager.pdf", width=10.75, height=9.25, onefile = TRUE)
sum_plots
dev.off()

