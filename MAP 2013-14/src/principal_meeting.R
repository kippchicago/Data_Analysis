require(ProjectTemplate)
load.project()

season1<-"Fall"
season2<-"Winter"
sy<-2014

s2s_match <- function(.data, season1="Fall", season2="Spring", sy=2013){
  
  # Filter to Season1
  m.1<-filter(.data, Season==season1, Year2==sy)
  
  # Filter to Season2
  m.2<-filter(.data, Season==season2, Year2==sy)


  # Join on ID and MeasurementScale
  m.12<-inner_join(m.1, m.2, by=c("StudentID", "MeasurementScale"))
  
  # construct and substitute names
  typical.growth <- as.name(paste0("Typical", season1, "To", season2, "Growth.x"))
  q<-substitute(typical.growth + TestRITScore.x)
  
  season.growth <- as.name(paste0(season1, "To", season2, "Growth.x"))
  q<-substitute(typical.growth + TestRITScore.x)
  
  m.12<-with(m.12, mutate(m.12, ProjectedGrowth=eval(q), MetTypical=TestRITScore.y>=ProjectedGrowth, GrowthSeason=paste(Season.x, Season.y, sep=" - ")))
  
  #return
  m.12
}

princ_meeting<-rbind(s2s_match(map.all, "Fall", "Winter", 2013),
      s2s_match(map.all, "Fall", "Spring", 2013),
      s2s_match(map.all, "Fall", "Winter", 2014)
      )


princ_meeting<-filter(princ_meeting, MeasurementScale %in% c("Reading", "Mathematics"))
princ_meeting.grouped<-group_by(princ_meeting,  GrowthSeason, MeasurementScale,  Year2.x, SchoolInitials.x, Grade.y)

hsr <- data.table(Grade.y = rep(c(0:3, 5:8),2),
                  MeasurementScale = c(rep("Reading", 8), rep("Mathematics", 8)),
                  TestRITScore.hsr = c(72, 57, 50, 63, 66, 63, 63, 69, 80, 68, 56, 73, 73, 70, 72, 65) 
  )

hsr.plot<-inner_join(filter(princ_plot.data, Year2.x==2014 & GrowthSeason=="Fall - Winter"), hsr, by=c("Grade.y", "MeasurementScale"))

perc_ME_grades<-summarise(princ_meeting.grouped, MetTypical=sum(MetTypical), N=n(),PctMEGrowth=MetTypical/n())
perc_ME_schools<-summarise(perc_ME_grades, Grade.y=as.integer(min(Grade.y)-1), MetTypical=sum(MetTypical), N=sum(N),PctMEGrowth=MetTypical/N)

princ_plot.data<-rbind(perc_ME_grades)

princ_plot.data<-data.table(princ_plot.data)



princ_plot.data[,GrowthSeason:=factor(GrowthSeason, levels=c("Fall - Winter", "Fall - Spring"))]
princ_plot.data[,GrowthSeasonInt:=as.numeric(GrowthSeason)]
princ_plot.data[Year2.x==2014, GrowthSeasonInt:=GrowthSeasonInt+.5]


princ_plot.data[,Schools:=factor(SchoolInitials.x,  levels=c("KAP", "KAMS", "KCCP", "KBCP"))]
princ_plot.data[Year2.x==2013, y:=Grade.y+.25]
princ_plot.data[Year2.x==2014, y:=Grade.y-.25]

princ_plot.data[Year2.x==2014,y.rect:=Grade.y-.5]
princ_plot.data[Year2.x==2014,yend.rect:=Grade.y+.5]
princ_plot.data[Year2.x==2014,x.rect:=-Inf]
princ_plot.data[Year2.x==2014,xend.rect:=Inf]

princ_plot.data[,rect.color:=Grade.y%%2==0]

princ_plot.data[Grade.y%%2==0, rect.color2:="gray"]
princ_plot.data[Grade.y%%2!=0, rect.color2:="white"]

princ_plot.data<-princ_plot.data[!(Schools=="KAMS" & Grade.y==5 & Year2.x==2013 & GrowthSeasonInt==1)]
princ_plot.data<-princ_plot.data[!(Schools=="KAMS" & Grade.y==6 & Year2.x==2013 & GrowthSeasonInt==1)]

princ_plot.data[Schools=="KAP" & Grade.y==3, y:=Grade.y]
princ_plot.data[Schools=="KCCP" & Grade.y==6, y:=Grade.y]
princ_plot.data[Schools=="KBCP" & Grade.y==5, y:=Grade.y]


#### April's Plot ####
cairo_pdf('graphs/Principal_Meeting_140218.pdf', width = 8.5, height=11)
  p<- ggplot(princ_plot.data[Year2.x==2013], aes(x=GrowthSeasonInt, y=y)) +
  geom_rect(data=princ_plot.data, aes(ymin=y.rect, 
                                      ymax=yend.rect, 
                                      xmin=x.rect, 
                                      xmax=xend.rect,
                                      fill=rect.color2), alpha=.2, color="white") +
      scale_fill_identity() + 
    geom_point(aes(color=Year2.x), size=8, alpha=.7) +  
    geom_line(aes(x=GrowthSeasonInt-.055,group=y), 
              arrow=arrow(type="closed", angle=15), 
              alpha=1, 
              size=1,
              color="gray") +
    geom_point(aes(color=Year2.x), size=8, alpha=1) +    
    geom_text(aes(label=round(PctMEGrowth*100,1)), size=2) +
    geom_point(data=princ_plot.data[Year2.x==2014], 
               aes(x=GrowthSeasonInt, 
                   color=Year2.x), 
               size=12, 
               alpha=1) +    
    geom_text(data=princ_plot.data[Year2.x==2014], 
              aes(x=GrowthSeasonInt,
                  label=round(PctMEGrowth*100,1)), 
              size=4, color="white") +
    geom_point(data=hsr.plot, aes(x=as.numeric(GrowthSeason)+1.25, 
                                  y=Grade.y,
                                  color="2013\nNetwork"),
               size=8, shape=15, 
               alpha=.7, 
               guide=T) +   
    geom_text(data=hsr.plot, aes(x=as.numeric(GrowthSeason)+1.25, 
                                 y=Grade.y, 
                                 label=TestRITScore.hsr),
              size=3) +
    facet_grid(Schools~MeasurementScale, scales="free_y", space="free_y") +
    scale_y_continuous("Grade", breaks=c(0:8)) +
    scale_x_continuous("",breaks=c(1,1.5, 2), labels=c("F12-W13", "F13-W14" ,"F12-S13"))  +
    scale_colour_manual(values=c("#BCD631", "#FEDA00", "#439539"), guide = guide_legend(title = "Year")) + 
    theme_bw() + 
    ggtitle("KIPP Chicago \nPercent Meeting/Exceeding Typical RIT Growth \n Fall-Winter 2013-14 vs. Fall-Winter  & Fall-Spring 2012-13")

p

dev.off()

#### April's Plot Reconfigured ####
princ_plot.data2<-dcast(princ_plot.data, formula = Schools + Grade.y + MeasurementScale + Year2.x + y.rect + yend.rect  + x.rect + xend.rect +  rect.color2 ~ GrowthSeason, value.var = "PctMEGrowth")

setnames(princ_plot.data2,c("Fall - Winter", "Fall - Spring"), c("x", "x.end"))
princ_plot.data2 <- data.table(princ_plot.data2)

princ_plot


cairo_pdf('graphs/Principal_Meeting_2_140219.pdf', width = 8.5, height=11)
  
p2<-  ggplot(princ_plot.data2, aes(x=x, y=Grade.y)) +
  geom_rect(data=princ_plot.data2, aes(ymin=y.rect, 
                                      ymax=yend.rect, 
                                      xmin=x.rect, 
                                      xmax=xend.rect,
                                      fill=rect.color2), alpha=.2, color="white") +
  scale_fill_identity() + 
  geom_vline(aes(xintercept=.8), 
             color="lightgray",
             size=1,
             linetype=3
             ) +
    geom_segment(aes(xend=x.end, yend=Grade.y),
              arrow=arrow(type="closed", 
                          angle=20,
                          length = unit(4, "points")),
              size=1.5,
              color="gray"
              ) + 
    geom_text(data=princ_plot.data2[Year2.x==2013], 
               aes(x=x-dodge, color=Year2.x, label=round(x*100)),
               size=4) + 
    geom_text(data=princ_plot.data2[Year2.x==2013], 
               aes(x=x.end+dodge, color=Year2.x, label=round(x.end*100)),
               size=4) + 
  geom_point(data=hsr.plot, aes(x=TestRITScore.hsr/100, y=Grade.y, color="Network"), size=7, shape="|") +
  geom_text(data=hsr.plot, aes(x=TestRITScore.hsr/100, y=Grade.y-.2, label=TestRITScore.hsr), color="#FEDA00", size=3) +
    geom_point(data=princ_plot.data2[Year2.x==2014], 
             aes(y=Grade.y+.25, color=Year2.x),
             fill="#439539",
             shape = 25,
             size=8,
             vjust=0) + 
  geom_text(data=princ_plot.data2[Year2.x==2014], 
             aes(y=Grade.y+.2, color=Year2.x, label=round(x*100)),
             color="white",
             size=4,
             vjust=0) + 
    scale_y_continuous("Grade", breaks=c(0:8)) +
    scale_x_continuous("Percent Meets/Exceeds Typical Growth", breaks=c(seq(.5,.9,by=.1)), labels=c("50%","60%", "70%", "80%", "90%")) +
    scale_color_manual(values = c("#BCD631", "#439539", "#FEDA00")) +
    #scale_fill_manual(values = c("#439539", "#BCD631")) +
    facet_grid(Schools~MeasurementScale, scales="free_y", space="free_y") +
  guides(fill=FALSE, shape=FALSE) + 
  ggtitle("KIPP Chicago \nPercent Meeting/Exceeding Typical RIT Growth \n Fall-Winter 2013-14 vs. Fall-Winter  & Fall-Spring 2012-13") +
    theme_bw()
  
p2

dev.off()
  
  
cairo_pdf('graphs/Principal_Meeting_Combined_140219.pdf', width = 8.5, height=11, onefile = T)
p
p2
dev.off()
