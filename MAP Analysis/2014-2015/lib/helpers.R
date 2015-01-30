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
           StudentName=paste(StudentFirstname.x,
                             StudentLastname.x),
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
  m.sub.scores<-.data %>% select(StudentID, 
                                           StudentFirstname,
                                           StudentLastname,
                                           SchoolInitials,
                                           Grade,
                                           MeasurementScale,
                                           TestRITScore,
                                           TestPercentile,
                                           TestQuartile,
                                           matches("(Goal)[0-9]RitScore")
  )
  
  
  
  m.sub.names<-.data %>% select(StudentID, 
                                          StudentFirstname,
                                          StudentLastname,
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
  )
  
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
  m.long.2<-m.long %>% filter(!is.na(value))
  assert_that(nrow(m.long)>=nrow(m.long.2))
  
  m.plot<-m.long.2 %>% mutate(Rank=rank(TestRITScore, ties.method="random"))
  
  m.plot<-m.plot %>% group_by(SchoolInitials, Grade, MeasurementScale, Goal_Name) %>%
    mutate(Rank2=rank(value, ties.method="random")) %>%
    mutate(StudentFullName=paste(StudentFirstname, StudentLastname)) %>%
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

