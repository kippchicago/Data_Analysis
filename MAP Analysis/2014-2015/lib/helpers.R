#plots a single line with term_first and term_second RIT PErcentilsfor each student overlayed over a line for every
# student in the class. Segmanets are colored by whether change in RIT 
# is stastitically greater than zero, not different than zero, or less than 
# zero.  dots (...) take arguments used by dplyr::filter.

pctl_change_plot<-function(.data, 
                           term_first, 
                           term_second, 
                           n_col=9, 
                           min_n=10,
                           ...){
  
  if(is.mapvizier(.data)) .data<-.data$mapData
  
  .data<-filter(.data, TermName %in% c(term_first, term_second), ...) %>%
    mutate(TermName=factor(as.character(TermName), 
                           levels=c(term_first, term_second)
    )
    )
  .data_joined<-inner_join(filter(.data, TermName==term_first), 
                           filter(.data, TermName==term_second),
                           by=c("StudentID", "MeasurementScale")
  )  %>%
    mutate(Diff_Pctl = TestPercentile.y-TestPercentile.x,
           Diff_Bool = Diff_Pctl>=0,
           Diff_RIT =  TestRITScore.y-TestRITScore.x,
           Diff_RIT_SE = sqrt(TestStandardError.y^2 + TestStandardError.x^2),
           Diff_RIT_Bool = (0>=-2*Diff_RIT_SE+Diff_RIT)&(0<=2*Diff_RIT_SE+Diff_RIT),
           Diff_RIT_Neg_Pos = ifelse(Diff_RIT_Bool==FALSE & Diff_RIT>0, "Positive",
                                     ifelse(Diff_RIT_Bool==FALSE & Diff_RIT<0, "Negative", "Zero")
           ),
           Diff_RIT_Neg_Pos=factor(Diff_RIT_Neg_Pos, levels=c("Zero", "Negative", "Positive")),
           StudentName=paste(StudentFirstName.x,
                             StudentLastName.x),
           Name=factor(StudentName, levels=unique(StudentName)[order(-Diff_Pctl)])
    )
  
  #str(.data_joined$Name) 
  
  if(nrow(.data_joined)<min_n) {
   return(message(paste("Returning without generating plot:\n",
                         "Number of students is fewer than",
                         min_n
                         )
                   )
   )
  }
  
  
  .data_joined_2<-select(.data_joined, -Name)
  

  #get min and max years
  min_year<-min(.data_joined$Year1.x)
  max_year<-max(.data_joined$Year2.y)
  
  #format terms
  t1<-gsub("(.+)\\s(.+)", "\\1\n\\2", term_first)
  t2<-gsub("(.+)\\s(.+)", "\\1\n\\2", term_second)
  p<-ggplot(.data_joined, 
            aes(x=Year1.x,
                y=TestPercentile.x)
  ) + 
    geom_segment(data=.data_joined_2, 
                 aes(xend=Year2.y,
                     yend=TestPercentile.y,
                     group=StudentID
                 ), 
                 alpha=.1) +
    geom_segment(aes(xend=Year2.y,
                     yend=TestPercentile.y,
                     group=StudentID,
                     color=Diff_RIT_Neg_Pos), 
                 size=2) +
    scale_x_continuous(name="Test Term", breaks=c(min_year, max_year), labels=c(t1,t2)) +
    scale_color_manual("RIT Difference\nis statistically:",values = c('#439539', #green
                                                                      '#F7941E', # Orange
                                                                      "purple"
                                                                        # True 
    )
    ) +
    facet_wrap(~Name,ncol = n_col) + 
    theme_bw() + 
    theme(axis.text.x=element_text(hjust=c(0,1))) +
    ylab("Test Percentile")
  
  p
  
  
  
}


##########
# Strands List Plot
#########

strands_list_plot <- function(.data, ...){

  if(is.mapvizier(.data)) .data <- as.data.frame(.data$mapData)
  .data <- filter(.data, ...)
  if(nrow(.data)==0){
    p_bad <- arrangeGrob(grid.text(label="Not enough info :-("),
                         ncol=1, nrow=1)
    return(p_bad)
  }
  m.sub.scores<-.data %>% select(StudentID, 
                                           StudentFirstName,
                                           StudentLastName,
                                           SchoolInitials,
                                           Grade,
                                           MeasurementScale,
                                           TestRITScore,
                                           TestPercentile,
                                           TestQuartile,
                                           matches("(Goal)[0-9]RitScore")
  )
  
  
  
  m.sub.names<-.data %>% select(StudentID, 
                                          StudentFirstName,
                                          StudentLastName,
                                          SchoolInitials,
                                          Grade,
                                          MeasurementScale,
                                          TestRITScore,
                                          TestPercentile,
                                          TestQuartile,
                                          matches("(Goal)[0-9]Name")
  )
  
  # melt scores
  m.melt.scores<-melt(m.sub.scores, 
                      id.vars=names(m.sub.scores)[1:9], 
                      measure.vars = names(m.sub.scores)[-c(1:9)]
  ) %>%
    mutate(value=as.numeric(value))
  
  m.melt.names<-melt(m.sub.names, 
                     id.vars=names(m.sub.names)[1:9],
                     measure.vars = names(m.sub.names)[-c(1:9)]
  )
  
  assert_that(nrow(m.melt.scores)==nrow(m.melt.names))
  
  # add homerooms
  #m.melt.scores2<-left_join(m.melt.scores, map_homerooms, by="StudentID")
  #assert_that(nrow(m.melt.scores)==nrow(m.melt.scores2))
  
  m.long <- m.melt.scores 
  m.long$Goal_Name<-m.melt.names$value
  
  
  
  assert_that(nrow(m.long)==nrow(m.melt.names))
  
  m.long.2<-m.long %>% filter(!is.na(Goal_Name))
  m.long.2<-m.long.2 %>% filter(!is.na(value))
  assert_that(nrow(m.long)>=nrow(m.long.2))
  
  m.plot<-m.long.2 %>% mutate(Rank=rank(TestRITScore, ties.method="random"))
  
  m.plot<-m.plot %>% group_by(SchoolInitials, Grade, MeasurementScale, Goal_Name) %>%
    mutate(Rank2=rank(value, ties.method="random")) %>%
    mutate(StudentFullName=paste(StudentFirstName, StudentLastName)) %>%
    filter(!is.na(Goal_Name)|is.na(value))

  x_max<-max(m.plot$value)+50
  x_min<-min(m.plot$value)
  
  p<-ggplot(data=m.plot, aes(y=Rank2, x=value)) +
    geom_point(aes(color=TestQuartile)) + 
    geom_text(aes(label=StudentFullName, 
                  color=TestQuartile), hjust=-0.3, size=2) +
    #geom_hline(aes(yintercept=mean(TestRITScore))) + 
    scale_color_discrete("Overall MAP Quartile") +
    facet_grid(.~Goal_Name) + 
    xlim(x_min,x_max) +
    xlab("RIT Score") +
    ylab("") +
    theme_bw() + 
    theme(strip.text.y=element_text(angle=270), 
          legend.position="bottom")
  
  p
  
}


## longitudnal summary plot

long_summary_plot<-function(map_summary_data, facets){
  p<-ggplot(map_summary_data, 
            aes(x=gsub("20","",SY), y=round(Pct_Typical*100)))+
    geom_line(aes(group=Subject, color=Subject)) + 
    geom_point(color="white", size=8.75) +
    geom_text(aes(label=paste(round(Pct_Typical*100),"%",sep=""), 
                  color=Subject),
              size=3) +
    facet_grid(facets) +
    scale_color_manual(values=c("#439539", "#255694")) +
    theme_bw()+
    xlab("School Year") + 
    ylab("% of students meeting/exceeding\ntypical growth") +
    theme(legend.position="none")
  
  p  
}



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
  
  if(nrow(map_test)==0){
    p<-grid.text(label = sprintf("Insufficient data for \n%s %s %s. :-(",
                                 school, grade, subject),
                 draw=FALSE)
  } else {
    p<-long_summary_plot(map_test,
                         facets= GrowthSeason ~ .) + 
      ggtitle("Historic Grade-level Growth")
  }
  
  
  # Becca Plots ####
  require(ensurer)
  # vectorize Andrew's mapvisuals::grade_level_season
  gls<-Vectorize(grade_level_season)
  
  cy <- 2015+12-grade
  
  data_bp<-map_mv$mapData %>% 
    filter(CohortYear==cy,
           MeasurementScale==subject,
           SchoolInitials==school)    
    
  
  
    
  
  
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
  
  if(nrow(data_bp)==0) {
    p_bp2<-grid.text(label = sprintf("Insufficient data for\n%s %s %s.",
                                    school, grade, subject),
                    draw=FALSE)
  } else {
    if(all(is.na(data_bp$TestPercentile))){
      p_bp2 <-grid.text(label = sprintf("NWEA doesn't provide percentiles\n for grade %s %s. :-(",
                                        grade, subject),
                        draw=FALSE)
    } else {
      data_bp2<-data_bp  %>%
        mutate(GradeSeason=Grade+gls(Season)) %>%
        select(GradeSeason,
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
        ) + 
        ggtitle("Historic Cohort Attainment")   
      
    }
    
  }
  # Page one ####
  require(gridExtra)
  
  message(paste("Plotting", school, subject, grade))
  plots<-arrangeGrob(p, 
                     p_bp2, ncol=2)
  
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


# Student Percentile Plots ####




student_npr_long_plot <- function(school, grade, subject){
  
  fsm<-Vectorize(fall_spring_me)
  fssm <- Vectorize(fall_spring_sort_me)
  
  
  map_individual<-map_current %>% filter(SchoolInitials==school,
                                         CohortYear==2015+12-grade,
                                         MeasurementScale==subject) 
  
  if(nrow(map_individual)==0){
    p_insufficent <- grid.text(label = sprintf("Insufficient data for\n%s %s %s.",
                                               school, grade, subject),
                               draw=FALSE)
    return(arrangeGrob(p_insufficent, nrow=1, ncol=1))
  }
  
  map_individual <- map_individual %>%
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