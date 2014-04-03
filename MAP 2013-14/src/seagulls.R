# Let's see some of the Spring-Fall-Winter "V" patterns 

# subset the last year to this year
m.dt <- rbind(map.all[Year2==2013 & Season=="Spring"],
              map.all[Year2==2014 & Season=="Fall"],
              map.all[Year2==2014 & Season=="Winter"]
)
m.dt<-m.dt[MeasurementScale %in% c("Mathematics", "Reading")]

m.dt[,Term:=factor(as.character(TermName), levels=c("Spring 2012-2013", 
                                                    "Fall 2013-2014", 
                                                    "Winter 2013-2014"))]
m.dt<-calc_quartile(m.dt, "TestPercentile", "TestQuartile")

m.mean.dt<-m.dt[,list(Avg=mean(TestRITScore), SD=sd(TestRITScore)), 
                by= list(Term, CohortYear, MeasurementScale)]

interval2 <- -qnorm((1-0.95)/2)


p<-ggplot(data=m.mean.dt[!CohortYear %in% c(2017,2021,2026) & MeasurementScale=="Mathematics"], aes(x=Term, y=TestRITScore)) + 
 # geom_line(aes(group=StudentID, 
 #               color=as.factor(TestQuartile)), 
#            alpha=.25) +
  geom_line(aes(x=Term, y=Avg, group=CohortYear)) + 
  geom_hline(data=m.mean.dt[Term=="Spring 2012-2013" & (!CohortYear %in% c(2017,2021,2026)) ],
           aes(yintercept=Avg), color="gray", type=3) +
  facet_grid(CohortYear ~ SchoolInitials, scales = "free_y") +
  theme_bw()

p

cairo_pdf("graphs/seagulls_v.pdf", width=8.5, height=11)
  p + ggtitle("Average Cohort RIT Scores\nSpring 13 | Fall 13 | Winter 14")
dev.off()
  
# Adding another year prior ####
# subset the last year to this year
m2.dt <- rbind(map.all[Year2==2012 & Season=="Spring"],
              map.all[Year2==2013],
              map.all[Year2==2014 & Season=="Fall"],
              map.all[Year2==2014 & Season=="Winter"]
)
m2.dt<-m2.dt[MeasurementScale %in% c("Mathematics", "Reading")]

m2.dt[,Term:=factor(as.character(TermName), 
                    levels=c("Spring 2011-2012", 
                             "Fall 2012-2013", 
                             "Winter 2012-2013",
                             "Spring 2012-2013", 
                             "Fall 2013-2014", 
                             "Winter 2013-2014"))]
m2.dt<-calc_quartile(m2.dt, "TestPercentile", "TestQuartile")

m2.mean.dt<-m2.dt[,list(Avg=mean(TestRITScore), SD=sd(TestRITScore)), 
                by= list(Term, CohortYear, MeasurementScale)]

p2<-ggplot(data=m2.mean.dt[!CohortYear %in% c(2016, 
                                             2017, 
                                             2020, 
                                             2021,
                                             2025,
                                             2026 
                                             )], aes(x=Term, y=TestRITScore)) + 
  # geom_line(aes(group=StudentID, 
  #               color=as.factor(TestQuartile)), 
  #            alpha=.25) +
  geom_line(aes(x=Term, y=Avg, group=CohortYear)) + 
  geom_hline(data=m2.mean.dt[Term=="Spring 2012-2013" & (!CohortYear %in% c(2016, 
                                                                           2017, 
                                                                           2020, 
                                                                           2021,
                                                                           2025,
                                                                           2026 
                                                                           )
                                                        )
                            ],
             aes(yintercept=Avg), color="gray", type=3) +
  geom_hline(data=m2.mean.dt[Term=="Spring 2011-2012" & (!CohortYear %in% c(2016, 
                                                                           2017, 
                                                                           2020, 
                                                                           2021,
                                                                           2025,
                                                                           2026 
  )
  )
  ],
  aes(yintercept=Avg), color="gray", type=3) +
  facet_grid(CohortYear ~ MeasurementScale, scales = "free_y") +
  theme_bw() + theme(axis.text.x=element_text(angle = 30, vjust = .5))

p2

cairo_pdf("graphs/seagulls_v_2.pdf", width=8.5, height=11)
 p2 + ggtitle("Average Cohort RIT Scores\nSpring 12 | Fall 12 | Winter 13\nSpring 13 | Fall 13 | Winter 12")
dev.off()
