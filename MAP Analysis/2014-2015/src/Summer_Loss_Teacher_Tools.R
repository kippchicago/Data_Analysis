require(ProjectTemplate)
load.project()

map_mv<-mapvizier(map_all)

#Let's try first with KCCP 7th graders
map_kccp<-filter(map_mv$mapData, 
       SchoolInitials=="KCCP", 
       TermName %in% c("Spring 2013-2014", "Fall 2014-2015")) %>%
  mutate(Name=paste(StudentFirstname, StudentLastname))

map_kccp_2<-inner_join(filter(map_kccp, Year2==2014), 
           filter(map_kccp, Year2==2015),
           by=c("StudentID", "MeasurementScale"))

map_kccp2 <- dplyr::select(map_kccp, -Name)

ggplot(filter(map_kccp, MeasurementScale=="Reading"), 
       aes(x=factor(TermName, levels=c("Spring 2012-2013", "Fall 2013-2014")), 
           y=TestPercentile)
       ) + 
  geom_line(data=filter(map_kccp2, MeasurementScale=="Reading"), 
            aes(group=StudentID), 
            alpha=.1) +
  geom_line(aes(group=StudentID), 
            color="orange",
            size=2) +
  facet_wrap(Name~MeasurementScale,ncol = 9)

pctl_change_plot<-function(.data, term_first, term_second, n_col=9,...){
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
           StudentName=paste(StudentFirstname.x,
                             StudentLastname.x),
           Name=factor(StudentName, levels=unique(StudentName)[order(-Diff_Pctl)])
           )
  
  #str(.data_joined$Name) 
  
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
    scale_color_manual("RIT Difference\nis statistically:",values = c('#F7941E', # False
                                  "purple",
                                  '#439539'  # True 
                                  )
                       ) +
    facet_wrap(~Name,ncol = n_col) + 
    theme_bw() + 
    theme(axis.text.x=element_text(hjust=c(0,1))) +
    ylab("Test Percentile")
  
  p
  
  
}


pdf(file="graphs/TT_test_S_to_F_loss_11x17.pdf", height=10.75, width=16.75)
pctl_change_plot(map_mv$mapData, 
                 "Spring 2012-2013", 
                 "Fall 2013-2014", 
                 n_col=9, 
                 MeasurementScale=="Reading", 
                 Grade %in% 5:6, 
                 SchoolInitials=="KCCP"
                 ) + 
  ggtitle("Spring 2013 to Fall 2013 Losses/Gains\nKCCP 6th Grade Reading")
dev.off()
