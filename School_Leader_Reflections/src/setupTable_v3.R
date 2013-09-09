# Aprils' Requested Data


require(ProjectTemplate)
load.project()

fs<-8 # set standard fontsize

#### Attendence ####



# Average Daily Attendence by School and by region
att.schools<-Attendence[, list(PctPresent=1-(sum(ABSENT)/.N)), by=SchoolInitials]

att.region<-Attendence[,list(SchoolInitials="Region", PctPresent=1-(sum(ABSENT)/.N))]
      
att.combined<-rbind(att.schools, att.region)
att.combined[,SchoolInitials:=factor(SchoolInitials, 
                                     levels=c("KAPS",
                                              "KAMS",
                                              "KCCP",
                                              "Region"),
                                     ordered=TRUE)]
setkey(att.combined, SchoolInitials)

# List of students with 13+ Days of School absent

Att.summary<-Attendence[,list(NAbsent=sum(ABSENT), 
                 NEnrolled=.N, 
                 PctAbsent=sum(ABSENT)/.N, 
                 PctPresent=1-(sum(ABSENT)/.N)), 
           by=list(LASTFIRST, 
                   SchoolInitials)]
absent.13plus <- Att.summary[, list(Pct13Plus = sum(NAbsent>=13)/.N, 
                                    N13plus= sum(NAbsent>=13)),
                             by=list(SchoolInitials)
                             ]

absent.13plus.region <- Att.summary[, list(SchoolInitials= "Region",
                                           Pct13Plus = sum(NAbsent>=13)/.N,
                                           N13plus= sum(NAbsent>=13))
                                    ]

att.13<-rbind(absent.13plus,absent.13plus.region)

att.13[,SchoolInitials:=factor(SchoolInitials, 
                                     levels=c("KAPS",
                                              "KAMS",
                                              "KCCP",
                                              "Region"),
                                     ordered=TRUE)]
setkey(att.13, SchoolInitials)


# Out of School Suspensions
susp<-Attendence[ATT_CODE=="S", list(DaysSuspended=sum(ABSENT)), by=list(SchoolInitials,LASTFIRST)]

att.susp<-rbind(susp[,list(NSuspended=.N), by=SchoolInitials], susp[,list(SchoolInitials="Region", NSuspended=.N)] )

att.susp[,SchoolInitials:=factor(SchoolInitials, 
                               levels=c("KAPS",
                                        "KAMS",
                                        "KCCP",
                                        "Region"),
                               ordered=TRUE)]
setkey(att.susp, SchoolInitials)



#In-School Suspensions

#Fights


#combined
att.tbl<-cbind(att.combined, 
               att.13[,2:3, with=FALSE], 
               att.susp[,2, with=FALSE])
att.tbl[,PctPresent:=round(PctPresent*100,1)]
att.tbl[,Pct13Plus:=round(Pct13Plus*100,1)]

setnames(att.tbl, c(" ", "ADA\n(%)", "13+ Days\nAbsent\n(% of all students)","13+ Days\nAbsent\n(#)", "Days\nSuspended\n(#)"))

att.gtbl<-tableGrob(att.tbl,  rows=NULL, show.rownames=FALSE, gpar.coltext    
           =gpar(fontsize=fs-.5), gpar.coretext    
           =gpar(fontsize=fs-.5))



## Interim/More Static Academic Data

#### MAP Data ####
# Number of students who meet their growth targets (NWEA standards)
# Number of students who exceed their growth targets (NWEA standards)
# Number of students who meet their growth targets (College ready standard)
# Number of students who exceed their growth targets (College ready standard)
# Absolute quartile performance

map.all<-copy(MAP.KIPP.Network.1213[Growth_Grade_Level %in% kc.grades])

map.all<-rbind(map.all[Growth_Grade_Level %in% c(0,2,5) 
                       & Growth.Season=="Fall to Spring", ],
               map.all[Growth_Grade_Level %in% c(1,6:8) 
                       & Growth.Season=="Spring to Spring", ]
               )

map.all[,School:=reorder(factor(School_Display_Name), Perc_Growth), by=list(Growth_Grade_Level, Sub_Test_Name)]

map.all[,School_Display_Name_Chi:=
          factor(School_Display_Name_Chi, 
                 levels=c("KIPP", 
                          "KIPP Network",
                          "National Norm",
                          "KIPP Ascend Middle School",
                          "KIPP Ascend Primary School",
                          "KIPP Create Middle School"))]

map.all[Sub_Test_Name=="Mathematics", Sub_Test_Name:="Math"]

##### bar again
map.bar.plot<-ggplotGrob(
  ggplot(map.all, aes(x=School, y=Perc_Growth)) +
    geom_bar(aes(fill=School_Display_Name_Chi), 
             stat='identity') +
    geom_text(data=map.all[School_Display_Name_Chi!="KIPP"],
              aes(y=Perc_Growth + 0.5,
                  label=Perc_Growth,
                  color=School_Display_Name_Chi),
              size=1.5,
              hjust=0) +
    coord_flip() +
    facet_grid(Sub_Test_Name ~ Growth_Grade_Level) +
    scale_fill_manual(values = c("#CFCCC1",  #All other KIPP
                                 "#C49A6C",  #KIPP National Average
                                 "#E27425",   #National Norm 
                                 "#439539",  #KAMS
                                 "purple",  #KAPS
                                 "#60A2D7"  #KCCP
                                  ) 
                    ) +
    scale_color_manual(values = c(#"#CFCCC1",  #All other KIPP
                                 "#C49A6C",  #KIPP National Average
                                 "#E27425",   #National Norm 
                                 "#439539",  #KAMS
                                 "purple",  #KAPS
                                 "#60A2D7"  #KCCP
    ) 
    ) +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_text(size=fs-.5),
          axis.ticks.y = element_blank(),
          axis.title = element_blank(),
          strip.text = element_text(size=fs-.5),
          legend.position="none")
)

#### MAP Growth ####

#create ranking for MAgnitude of Growth
map.all[,Mag_Rank:=reorder(factor(School_Display_Name), 
                           Mag_Growth), 
        by=list(Growth_Grade_Level, Sub_Test_Name)]

map.mag.plot<-ggplotGrob(
    ggplot(map.all, aes(x=Mag_Growth, y=Mag_Rank)) +  
   geom_segment(aes(x=0,
                   xend=Mag_Growth, 
                   yend=Mag_Rank , 
                   color=School_Display_Name_Chi), 
               arrow=arrow(length=unit(.1, "char"), 
                           type="closed"), 
               size=.25) +
    geom_text(data=map.all[School_Display_Name_Chi!="KIPP"],
              aes(x=Mag_Growth+.2,
                  label=Mag_Growth,
                  color=School_Display_Name_Chi),
              size=1,
              hjust=0) +
    facet_grid(Sub_Test_Name ~ Growth_Grade_Level) +
    scale_fill_manual(values = c("#CFCCC1",  #All other KIPP
                                 "#C49A6C",  #KIPP National Average
                                 "#E27425",  #National Norm 
                                 "#439539",  #KAMS
                                 "purple",   #KAPS
                                 "#60A2D7"   #KCCP
    ) 
    ) +
    scale_color_manual(values = c("#CFCCC1",  #All other KIPP
                                  "#C49A6C",  #KIPP National Average
                                  "#E27425",  #National Norm 
                                  "#439539",  #KAMS
                                  "purple",   #KAPS
                                  "#60A2D7"   #KCCP
    ) 
    ) +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_text(size=fs-.5),
          axis.ticks.y = element_blank(),
          axis.title = element_blank(),
          strip.text = element_text(size=fs-.5),
          legend.position="none")
)


#### ISAT ####
# Number of students who meet or exceed standards

ISAT.58.Comp.plot<-ggplot(ISAT.plotdata[Year>=2012 
                                        & Grade=="5-8"
                                        & variable=="Composite"], 
                          aes(x=Year, y=value)) +
  geom_line(aes(group=School, color=School)) + 
  geom_point(data=ISAT.plotdata[School=="KCCP" 
                                & Grade=="5-8" 
                                & variable=="Composite"], 
             aes(x=Year, y=value, color=School), shape=18, size=3.5) +
  #geom_point(data=ISAT.maxmin[Year >= 2012 & Grade == "5-8"
  #                            & variable=="Composite"], 
  #           aes(x=Year, y=value, color=grp)) + 
  facet_grid(Grade ~ variable) + 
  scale_color_manual(values = c("#E27425", #KAMS
                                "#439539", #CPS 
                                "#60A2D7", #KCCP
                                "#8D8685", #max
                                "#8D8685" #min
  )
  ) +
  scale_x_discrete(breaks=c("2012", "2013"))+
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x= element_text(size= fs-2), #,angle=35),
        axis.text.y= element_text(size= fs-2),
        strip.text = element_text(size=fs),
        plot.margin = unit(c(0.5, 0, 0.5, 0.5), "lines")
  ) 




ISAT.58.Comp.tbl<-ggplot(ISAT.plotdata[Year>=2007 
                                       & Grade %in% c("5","6","7","8")
                                       & variable!="Composite"], 
                         aes(x=Year, y=y.label.pos)) + 
  geom_text(aes(label=round(value), color=School), size=2.5) + 
  facet_grid(Grade~variable) + 
  #theme_bw +
  theme(panel.grid.major = element_blank(), 
        #panel.border = element_blank(), 
        axis.text.y =  element_blank(),
        axis.ticks =  element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(), 
        axis.text.x= element_text(size= fs-2), #,angle=35),
        strip.text = element_text(size=fs),
        #strip.background = element_blank(),
        legend.position="none",
        plot.margin = unit(c(0.5, 0.5, 0.5, -1), "lines")
  ) + 
  ylim(0.5, -0.5) +
  scale_color_manual(values = c("#E27425", #CPS 
                                "#439539", #KAMS
                                "#60A2D7", #KCCP
                                "#BCD631", #max
                                "#BCD631"  #min
                                )
                     )






#### Enrollment  ####
#Enrollment - by grade
#FRM Data
#Ethnicity Data
#Special Ed Datai



Enroll.g<-ggplotGrob(ggplot(Enrollment.plotdata) +
  geom_hline(data=Enrollment.budgeted, 
             aes(yintercept=Budget), 
             color="#E27425",
             alpha=0.6
  ) +
  geom_step(aes(x=Date, y=N, color=SchoolInitials), size=.5) + 
  facet_grid(Grade~SchoolInitials, scales="free_y") +
  scale_color_manual(values = c("purple",  #KCCP 
                                "#439539", #KAMS
                                "#60A2D7" #KCCP
  )
  ) +
  theme(legend.position="none",
        axis.title=element_blank(),
        axis.text.y=element_text(size=fs-4.5),
        axis.text=element_text(size=fs-2),
        strip.text = element_text(size=fs-1)
  )
)


#### Transfers #### 
transfer.gtbl<-tableGrob(Xfer.table, show.rownames=FALSE, 
                         gpar.coretext=gpar(fontsize=8),
                         gpar.coltext=gpar(fontsize=8))

xferplot.1213<-data.table(xferplot.1213)

xferplot.1213[,School:=factor(School, 
                              levels=c("KAPS", "KAMS", "KCCP"), 
                              ordered=T)]

t10pct.df<-data.frame(School=c("KAMS","KAPS", "KCCP"), Month="May", Variable="10% Limit", Value=c(30, 28,7))

tly.df<-data.frame(School=c("KAMS", "KAPS"), Month="May", Variable="Last Year", Value=c(12,5))

transfer.plot<-ggplotGrob(
  ggplot(data=subset(xferplot.1213, Variable=="Ceiling"), 
         aes(x=Month, y=Value)) + 
    geom_line(aes(group=Variable), color="#E27425") + 
    geom_line(data=subset(xferplot.1112, Variable!="Ceiling"), 
              aes(x=Month, y=CumVal, group=Variable), 
              color="#17345B") + 
    geom_bar(data=subset(xferplot.1213, Variable!="Ceiling"), 
             aes(x=Month, y=Value, fill=School), 
             stat='identity', 
             #fill="#439539", 
             width=.5) + 
    geom_text(data=subset(xferplot.1213, Variable!="Ceiling"), 
              aes(x=Month, y=Value-.5, group=Variable, label=Value), 
              size=fs-6,
              vjust=1) +
    facet_grid(School~., scale="free_y") +
    scale_fill_manual(values = c("purple",  #KCCP 
                                 "#439539", #KAMS
                                 "#60A2D7" #KCCP
    )
    ) +
    geom_text(data=t10pct.df, 
              aes(label=Variable), 
              color="#E27425", size=fs-6) + 
    geom_text(data=tly.df, 
              aes(label=Variable), 
              color="#17345B", size=fs-6) +
    theme(axis.title = element_blank(),
          axis.text = element_text(size=fs-.5),
          strip.text = element_text(size=fs),
          legend.position="none"
    )      
                            
                            )



#Staff Satisfaction and Retention
#Q12
#% of teachers returning
#healthy schools and regions (parent satisfaction, 
#Will get pdfs of healthy schools and regions surveys.
###OPTIONAL!!!!###

### Special Ed Data
## SPED referrals?
# of students who start the year in SPED
# of students who are referred into SPED
# of students who are exited from SPED


