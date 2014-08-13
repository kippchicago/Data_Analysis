require(ProjectTemplate)
load.project()

map_mv<-mapvizier(map_all)

map_kccp<-filter(map_mv$mapData, 
       SchoolInitials=="KCCP", 
       TermName %in% c("Spring 2012-2013", "Fall 2013-2014")) %>%
  mutate(Name=paste(StudentFirstname, StudentLastname))

map_kccp_2<-inner_join(filter(map_kccp, Year2==2013), 
           filter(map_kccp, Year2==2014),
           by=c("StudentID", "MeasurementScale"))

map_kccp2 <- select(map_kccp, -Name)

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
           StudentName=paste(StudentFirstname.x,
                             StudentLastname.x),
           Name=factor(StudentName, levels=unique(StudentName)[order(-Diff_Pctl)])
           )
  
  #str(.data_joined$Name) 
  
  .data_joined_2<-select(.data_joined, -Name)

  
  p<-ggplot(.data_joined, 
            aes(x=Year2.x,
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
                     group=StudentID), 
              color="orange",
              size=2) +
    facet_wrap(~Name,ncol = n_col) + 
    theme_bw() + 
    theme()
  
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
