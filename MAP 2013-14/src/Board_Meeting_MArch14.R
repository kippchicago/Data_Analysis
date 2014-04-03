require(ProjectTemplate)
load.project()

season1<-"Fall"
season2<-"Winter"
sy<-2014



princ_meeting<-rbind(s2s_match(map.all, "Fall", "Winter", 2013),
      s2s_match(map.all, "Fall", "Spring", 2013),
      s2s_match(map.all, "Fall", "Winter", 2014)
      )


princ_meeting<-filter(princ_meeting, MeasurementScale %in% c("Reading", "Mathematics"))
princ_meeting.grouped<-group_by(princ_meeting,  GrowthSeason, MeasurementScale,  Year2.x, SchoolInitials.x, Grade.y)

hsr <- data.table(Grade.y = rep(c(0:3, 5:8,10),2),
                  MeasurementScale = c(rep("Reading", 9), rep("Mathematics", 9)),
                  TestRITScore.hsr = c(72, 57, 50, 63, 66, 63, 63, 69, 62, 80, 68, 56, 73, 73, 70, 72, 65, 70) 
  )

perc_ME_grades<-summarise(princ_meeting.grouped, MetTypical=sum(MetTypical), N=n(),PctMEGrowth=MetTypical/n())
perc_ME_schools<-summarise(perc_ME_grades, Grade.y=as.integer(min(Grade.y)-1), MetTypical=sum(MetTypical), N=sum(N),PctMEGrowth=MetTypical/N)

princ_plot.data<-rbind(perc_ME_grades)
princ_plot.data$Grade.y<-as.numeric(princ_plot.data$Grade.y)


princ_plot.data<-data.table(princ_plot.data)

#getting regional summary
perc_ME_region<-summarise(perc_ME_schools, SchoolInitials.x="Region", Grade.y=10, MetTypical=sum(MetTypical), N=sum(N),PctMEGrowth=MetTypical/N)
princ_plot.data<-rbind(perc_ME_grades, perc_ME_region)
princ_plot.data<-data.table(princ_plot.data)


princ_plot.data[,GrowthSeason:=factor(GrowthSeason, levels=c("Fall - Winter", "Fall - Spring"))]
princ_plot.data[,GrowthSeasonInt:=as.numeric(GrowthSeason)]
princ_plot.data[Year2.x==2014, GrowthSeasonInt:=GrowthSeasonInt+.5]


princ_plot.data[,Schools:=factor(SchoolInitials.x,  levels=c("KAMS", "KCCP", "KBCP", "KAP", "Region"))]


hsr.plot<-inner_join(filter(princ_plot.data, Year2.x==2014 & GrowthSeason=="Fall - Winter"), hsr, by=c("Grade.y", "MeasurementScale"))


princ_plot.data[Year2.x==2013, y:=Grade.y+.25]
princ_plot.data[Year2.x==2014, y:=Grade.y-.25]

princ_plot.data[Year2.x==2014,y.rect:=Grade.y-.5]
princ_plot.data[Year2.x==2014,yend.rect:=Grade.y+.5]
princ_plot.data[Year2.x==2014,x.rect:=-Inf]
princ_plot.data[Year2.x==2014,xend.rect:=Inf]

princ_plot.data[,rect.color:=Grade.y%%2==0]

princ_plot.data[Grade.y%%2==0, rect.color2:="gray"]
princ_plot.data[Grade.y%%2!=0, rect.color2:="white"]
princ_plot.data[Grade.y==10,rect.color2:="60A2D7"]

princ_plot.data<-princ_plot.data[!(Schools=="KAMS" & Grade.y==5 & Year2.x==2013 & GrowthSeasonInt==1)]
princ_plot.data<-princ_plot.data[!(Schools=="KAMS" & Grade.y==6 & Year2.x==2013 & GrowthSeasonInt==1)]

princ_plot.data[Schools=="KAP" & Grade.y==3, y:=Grade.y]
princ_plot.data[Schools=="KCCP" & Grade.y==6, y:=Grade.y]
princ_plot.data[Schools=="KBCP" & Grade.y==5, y:=Grade.y]




#### April's Plot Reconfigured ####
princ_plot.data2<-dcast(princ_plot.data, formula = Schools + Grade.y + MeasurementScale + Year2.x + y.rect + yend.rect  + x.rect + xend.rect +  rect.color2 ~ GrowthSeason, value.var = "PctMEGrowth")

setnames(princ_plot.data2,c("Fall - Winter", "Fall - Spring"), c("x", "x.end"))
princ_plot.data2 <- data.table(princ_plot.data2)

princ_plot.data2[,dodge:=ifelse(x.end-x>=0, .025,-.025)]
princ_plot.data2[is.na(x) & x.end>=0, dodge:=.025]

princ_plot.data2[Grade.y==10,rect.color2:="#60A2D7"]




p2<-  ggplot(princ_plot.data2, aes(x=x, y=Grade.y)) +
  geom_rect(data=princ_plot.data2, aes(ymin=y.rect, 
                                       ymax=yend.rect, 
                                       xmin=x.rect, 
                                       xmax=xend.rect,
                                       fill=rect.color2), alpha=.2, color="white") +
  scale_fill_identity() + 
  geom_vline(aes(xintercept=.8,linetype="2013-14 Goal"), 
             color="lightgray",
             size=1
  ) +
  geom_segment(aes(xend=x.end, yend=Grade.y, linetype="Winter 12 to Spring 13"),
               arrow=arrow(type="closed", 
                           angle=20,
                           length = unit(4, "points")),
               size=1.5,
               color="gray"
  ) + 
  geom_point(data=hsr.plot, 
             aes(x=TestRITScore.hsr/100, y=Grade.y, color="2012-13 KIPP Network (Spring)"), 
             size=7, shape="|", 
             show_guide=FALSE) +
  geom_text(data=princ_plot.data2[Year2.x==2013], 
            aes(x=x-dodge, color="2012-13 (Winter & Spring)", label=round(x*100)),
            size=4) + 
  geom_text(data=princ_plot.data2[Year2.x==2013], 
            aes(x=x.end+dodge, color="2012-13 (Winter & Spring)", label=round(x.end*100)),
            size=4) + 
  geom_text(data=hsr.plot, aes(x=TestRITScore.hsr/100, y=Grade.y-.2, label=TestRITScore.hsr), color="#FEDA00", size=3) +
  geom_point(data=princ_plot.data2[Year2.x==2014], 
             aes(y=Grade.y+.25, color="2013-14 (Winter Only)"),
             fill="#439539",
             shape = 18,
             size=9,
             vjust=0, 
             show_guide=FALSE) + 
  geom_text(data=princ_plot.data2[Year2.x==2014], 
            aes(y=Grade.y+.2, color=Year2.x, label=round(x*100)),
            color="white",
            size=4,
            vjust=0) + 
  scale_y_continuous("Grade", breaks=c(0:8)) +
  scale_x_continuous("Percent Meets/Exceeds Typical Growth", breaks=c(seq(.5,.9,by=.1)), labels=c("50%","60%", "70%", "80%", "90%")) +
  scale_color_manual("SY-Season",
                     values = c("#BCD631",  # 2013
                                "#FEDA00",  # Network
                                "#439539"   # 2014
                                ), 
                     breaks=c("2012-13 KIPP Network (Spring)", 
                              "2012-13 (Winter & Spring)", 
                              "2013-14 (Winter Only)"
                              ),
                     guide = guide_legend(reverse=TRUE)
                     ) + 
  scale_linetype_manual("", values=c(3,1), guide = guide_legend(reverse=TRUE)) +
  #scale_fill_manual(values = c("#439539", "#BCD631")) +
  facet_grid(Schools~MeasurementScale, scales="free_y", space="free_y") +
  guides(color=guide_legend(order=1), linetype=guide_legend(order=2), fill=FALSE, shape=FALSE) + 
  ggtitle("KIPP Chicago \nPercent Meeting/Exceeding Typical RIT Growth \n Fall-Winter 2013-14 vs. Fall-Winter  & Fall-Spring 2012-13") +
  theme_bw() +
  theme(legend.position="bottom")

p2



cairo_pdf('graphs/BOD_Meeting_2_140306.pdf', width = 8.5, height=11)
  
p2

dev.off()
  
  
cairo_pdf('graphs/Principal_Meeting_Combined_140219.pdf', width = 8.5, height=11, onefile = T)
p
p2
dev.off()




#____________________#
# College Ready ######
#____________________#

map.dt <- data.table(map.all)
map.dt[,TestQuartile:=as.factor(1+ floor(TestPercentile/25))]

map.dt<-calc_tiered_growth(map.dt, "TestQuartile", "Grade")


# Fix the Fall to spring to have the right project growth here. 

bod_meeting<-rbind(s2s_match(map.dt, "Fall", "Winter", 2013),
                     s2s_match(map.dt, "Fall", "Spring", 2013),
                     s2s_match(map.dt, "Fall", "Winter", 2014)
)

bod_meeting<-mutate(bod_meeting, ProjectedCRGrowth=TestRITScore.x + (ifelse(GrowthSeason=="Fall - Winter", TypicalFallToWinterGrowth.x, TypicalFallToSpringGrowth.x)*KIPPTieredGrowth.x),  MetCR=TestRITScore.y>=ProjectedCRGrowth)


bod_meeting<-filter(bod_meeting, MeasurementScale %in% c("Reading", "Mathematics"))
bod_meeting.grouped<-group_by(bod_meeting,  GrowthSeason, MeasurementScale,  Year2.x, SchoolInitials.x, Grade.y)


perc_CR_grades<-summarise(bod_meeting.grouped, MetCR=sum(MetCR), N=n(),PctCRGrowth=MetCR/n())
perc_CR_schools<-summarise(perc_CR_grades, Grade.y=as.integer(min(Grade.y)-1), MetCR=sum(MetCR), N=sum(N),PctCRGrowth=MetCR/N)

#getting regional summary
perc_CR_region<-summarise(perc_CR_schools, SchoolInitials.x="Region", Grade.y=10, MetCR=sum(MetCR), N=sum(N),PctCRGrowth=MetCR/N)
bod_plot.data<-rbind(perc_CR_grades, perc_CR_region)
bod_plot.data<-data.table(bod_plot.data)


bod_plot.data[,GrowthSeason:=factor(GrowthSeason, levels=c("Fall - Winter", "Fall - Spring"))]
bod_plot.data[,GrowthSeasonInt:=as.numeric(GrowthSeason)]
bod_plot.data[Year2.x==2014, GrowthSeasonInt:=GrowthSeasonInt+.5]


bod_plot.data[,Schools:=factor(SchoolInitials.x,  levels=c("KAMS", "KCCP", "KBCP", "KAP", "Region"))]
bod_plot.data[Year2.x==2013, y:=Grade.y+.25]
bod_plot.data[Year2.x==2014, y:=Grade.y-.25]

bod_plot.data[Year2.x==2014,y.rect:=Grade.y-.5]
bod_plot.data[Year2.x==2014,yend.rect:=Grade.y+.5]
bod_plot.data[Year2.x==2014,x.rect:=-Inf]
bod_plot.data[Year2.x==2014,xend.rect:=Inf]

bod_plot.data[,rect.color:=Grade.y%%2==0]

bod_plot.data[Grade.y%%2==0, rect.color2:="gray"]
bod_plot.data[Grade.y%%2!=0, rect.color2:="white"]
bod_plot.data[Grade.y==10,rect.color2:="60A2D7"]

bod_plot.data<-bod_plot.data[!(Schools=="KAMS" & Grade.y==5 & Year2.x==2013 & GrowthSeasonInt==1)]
bod_plot.data<-bod_plot.data[!(Schools=="KAMS" & Grade.y==6 & Year2.x==2013 & GrowthSeasonInt==1)]

bod_plot.data[Schools=="KAP" & Grade.y==3, y:=Grade.y]
bod_plot.data[Schools=="KCCP" & Grade.y==6, y:=Grade.y]
bod_plot.data[Schools=="KBCP" & Grade.y==5, y:=Grade.y]




#### April's Plot Reconfigured ####
bod_plot.data2<-dcast(bod_plot.data, formula = Schools + Grade.y + MeasurementScale + Year2.x + y.rect + yend.rect  + x.rect + xend.rect +  rect.color2 ~ GrowthSeason, value.var = "PctCRGrowth")

setnames(bod_plot.data2,c("Fall - Winter", "Fall - Spring"), c("x", "x.end"))
bod_plot.data2 <- data.table(bod_plot.data2)

bod_plot.data2[,dodge:=ifelse(x.end-x>=0, .025,-.025)]
bod_plot.data2[is.na(x) & x.end>=0, dodge:=.025]

bod_plot.data2[Grade.y==10,rect.color2:="#60A2D7"]




p3<-  ggplot(bod_plot.data2, aes(x=x, y=Grade.y)) +
  geom_rect(data=bod_plot.data2, aes(ymin=y.rect, 
                                       ymax=yend.rect, 
                                       xmin=x.rect, 
                                       xmax=xend.rect,
                                       fill=rect.color2), alpha=.2, color="white") +
  scale_fill_identity() + 
  geom_vline(aes(xintercept=.5,linetype="2013-14 Goal"), 
             color="lightgray",
             size=1
  ) +
  geom_segment(aes(xend=x.end, yend=Grade.y, linetype="Winter 12 to Spring 13"),
               arrow=arrow(type="closed", 
                           angle=20,
                           length = unit(4, "points")),
               size=1.5,
               color="gray"
  ) + 
  geom_text(data=bod_plot.data2[Year2.x==2013], 
            aes(x=x-dodge, color="2012-13 (Winter & Spring)", label=round(x*100)),
            size=4) + 
  geom_text(data=bod_plot.data2[Year2.x==2013], 
            aes(x=x.end+dodge, color="2012-13 (Winter & Spring)", label=round(x.end*100)),
            size=4) + 
  geom_point(data=bod_plot.data2[Year2.x==2014], 
             aes(y=Grade.y+.25, color="2013-14 (Winter Only)"),
             fill="#439539",
             shape = 18,
             size=9,
             vjust=0, 
             show_guide=FALSE) + 
  geom_text(data=bod_plot.data2[Year2.x==2014], 
            aes(y=Grade.y+.2, color=Year2.x, label=round(x*100)),
            color="white",
            size=4,
            vjust=0) + 
  scale_y_continuous("Grade", breaks=c(0:8)) +
  scale_x_continuous("Percent Meets/Exceeds College Ready Growth", breaks=c(seq(.2,.9,by=.1)), labels=c("20%", "30%","40%", "50%","60%", "70%", "80%", "90%")) +
  scale_color_manual("SY-Season",
                     values = c("#60A2D7",  # 2013
                                #"#FEDA00",  # Network
                                "#255694"   # 2014
                     ), 
                     breaks=c("2012-13 KIPP Network (Spring)", 
                              "2012-13 (Winter & Spring)", 
                              "2013-14 (Winter Only)"
                     ),
                     guide = guide_legend(reverse=TRUE)
  ) + 
  scale_linetype_manual("", values=c(3,1), guide = guide_legend(reverse=TRUE)) +
  #scale_fill_manual(values = c("#439539", "#BCD631")) +
  facet_grid(Schools~MeasurementScale, scales="free_y", space="free_y") +
  guides(color=guide_legend(order=1), linetype=guide_legend(order=2), fill=FALSE, shape=FALSE) + 
  ggtitle("KIPP Chicago \nPercent Meeting/Exceeding College Ready RIT Growth \n Fall-Winter 2013-14 vs. Fall-Winter  & Fall-Spring 2012-13") +
  theme_bw() +
  theme(legend.position="bottom")

p3



cairo_pdf('graphs/Boardl_Meeting_CR_only_140306.pdf', width = 8.5, height=11)

p3

dev.off()

cairo_pdf('graphs/Board_Meeting_140306.pdf', width = 8.5, height=11, onefile = T)

p2

p3

dev.off()

