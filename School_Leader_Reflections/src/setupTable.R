# Aprils' Requested Data


require(ProjectTemplate)
load.project()
################
## Attendence ##
################

# set fontsize
fs<-8

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
           =gpar(fontsize=fs), gpar.coretext    
           =gpar(fontsize=fs))


###################
## Interim/More Static Academic Data
################

## MAP Data (ignore below see my chart)
# Number of students who meet their growth targets (NWEA standards)
# Number of students who exceed their growth targets (NWEA standards)
# Number of students who meet their growth targets (College ready standard)
# Number of students who exceed their growth targets (College ready standard)
# Absolute quartile performance

#map.summary.1213<-map.1213[,list(pct.meets=sum(Meets)/.N, tot.meets=sum(Meets),  pct.50th.fall=sum(Fall12_Above50th)/.N, tot.50th.fall=sum(Fall12_Above50th), pct.50th.spring=sum(Spring13_Above50th)/.N, tot.50th.spring=sum(Spring13_Above50th), tot.students=.N),by=list(Subject, Fall12_Grade, SchoolInitials)]



map.tbl<-cast(map.summary.1213[Subject!="General Science", list(Pct=round(sum(tot.meets)/sum(tot.students)*100)), by=list(SchoolInitials, Subject)], Subject~SchoolInitials)

map.kippchi.tbl<-cast(map.summary.1213[Subject!="General Science", list(SchoolInitials="KIPP Chicago", Pct=round(sum(tot.meets)/sum(tot.students)*100)), by=list(Subject)], Subject~SchoolInitials)

#add KIPP Network Referenc

map.kippchi.tbl<-cbind(map.kippchi.tbl, data.table(KIPP=c(67, 59))) 

setnames(map.kippchi.tbl, c("KIPP Chicago","KIPP"), c("KIPP\nChicago","KIPP\nNetwork"))

map.kippchi.gtbl<-tableGrob(map.kippchi.tbl[,2:3], rows=NULL, show.rownames=FALSE, gpar.coltext    
           =gpar(fontsize=fs), gpar.coretext    
           =gpar(fontsize=fs))
# GROWTH

#now for K-2
map.kaps.tbl<-cast(map.summary.1213[Subject!="General Science" & SchoolInitials=="KAPS", list(Pct=round(sum(tot.meets)/sum(tot.students)*100)), by=list(SchoolInitials, Subject)], Subject~SchoolInitials)

map.kaps.tbl<-cbind(map.kaps.tbl, data.table(KIPP=c(62, 56))) 

setnames(map.kaps.tbl, "KIPP", "KIPP\nNetwork")

map.kaps.gtbl<-tableGrob(map.kaps.tbl, rows=NULL, show.rownames=FALSE, gpar.coltext    
                            =gpar(fontsize=fs), gpar.coretext    
                            =gpar(fontsize=fs))

#KAMS
map.kams.tbl<-cast(map.summary.1213[Subject!="General Science" & SchoolInitials=="KAMS", list(Pct=round(sum(tot.meets)/sum(tot.students)*100)), by=list(SchoolInitials, Subject)], Subject~SchoolInitials)

map.kams.tbl<-cbind(map.kams.tbl, data.table(KIPP=c(70, 61))) 

setnames(map.kams.tbl, "KIPP", "KIPP\nNetwork")

map.kams.gtbl<-tableGrob(map.kams.tbl[,2:3], rows=NULL, show.rownames=FALSE, gpar.coltext    
                         =gpar(fontsize=fs), gpar.coretext    
                         =gpar(fontsize=fs))

#KCCP
map.kccp.tbl<-cast(map.summary.1213[Subject!="General Science" & SchoolInitials=="KCCP", list(Pct=round(sum(tot.meets)/sum(tot.students)*100)), by=list(SchoolInitials, Subject)], Subject~SchoolInitials)

map.kccp.tbl<-cbind(map.kccp.tbl, data.table(KIPP=c(70, 64))) 

setnames(map.kccp.tbl, "KIPP", "KIPP\nNetwork")

map.kccp.gtbl<-tableGrob(map.kccp.tbl[,2:3], rows=NULL, show.rownames=FALSE, gpar.coltext    
                         =gpar(fontsize=fs), gpar.coretext    
                         =gpar(fontsize=fs))

## Above average
#now for K-2
map.kaps.above.tbl<-cast(map.summary.1213[Subject!="General Science" & SchoolInitials=="KAPS", list(Pct=round(sum(tot.50th.spring)/sum(tot.students)*100)), by=list(SchoolInitials, Subject)], Subject~SchoolInitials)

map.kaps.above.tbl<-cbind(map.kaps.above.tbl, data.table(KIPP=c(49, 52))) 

setnames(map.kaps.above.tbl, "KIPP", "KIPP\nNetwork")

map.kaps.above.gtbl<-tableGrob(map.kaps.above.tbl, rows=NULL, show.rownames=FALSE, gpar.coltext    
                         =gpar(fontsize=fs), gpar.coretext    
                         =gpar(fontsize=fs))

#KAMS
map.kams.above.tbl<-cast(map.summary.1213[Subject!="General Science" & SchoolInitials=="KAMS", list(Pct=round(sum(tot.50th.spring)/sum(tot.students)*100)), by=list(SchoolInitials, Subject)], Subject~SchoolInitials)

map.kams.above.tbl<-cbind(map.kams.above.tbl, data.table(KIPP=c(48, 48))) 

setnames(map.kams.above.tbl, "KIPP", "KIPP\nNetwork")

map.kams.above.gtbl<-tableGrob(map.kams.above.tbl[,2:3], rows=NULL, show.rownames=FALSE, gpar.coltext    
                               =gpar(fontsize=fs), gpar.coretext    
                               =gpar(fontsize=fs))

#KCCP
map.kccp.above.tbl<-cast(map.summary.1213[Subject!="General Science" & SchoolInitials=="KCCP", list(Pct=round(sum(tot.50th.spring)/sum(tot.students)*100)), by=list(SchoolInitials, Subject)], Subject~SchoolInitials)

map.kccp.above.tbl<-cbind(map.kccp.above.tbl, data.table(KIPP=c(49, 52))) 

setnames(map.kccp.above.tbl, "KIPP", "KIPP\nNetwork")

map.kccp.above.gtbl<-tableGrob(map.kccp.above.tbl[,2:3], rows=NULL, show.rownames=FALSE, gpar.coltext    
                               =gpar(fontsize=fs), gpar.coretext    
                               =gpar(fontsize=fs))


#add KIPP CHI
map.kippchi.above.tbl<-cast(map.summary.1213[Subject!="General Science", list(SchoolInitials="KIPP Chicago", Pct=round(sum(tot.50th.spring)/sum(tot.students)*100)), by=list(Subject)], Subject~SchoolInitials)



map.kippchi.above.tbl<-cbind(map.kippchi.above.tbl, data.table(KIPP=c(48, 49))) 

setnames(map.kippchi.above.tbl, c("KIPP Chicago","KIPP"), c("KIPP\nChicago","KIPP\nNetwork"))

map.kippchi.above.gtbl<-tableGrob(map.kippchi.above.tbl[,2:3], rows=NULL, show.rownames=FALSE, gpar.coltext    
                            =gpar(fontsize=fs), gpar.coretext    
                            =gpar(fontsize=fs))


#ISAT
# Number of students who meet or exceed standards

ISAT.58.Comp.plot<-ggplot(ISAT.plotdata[Year>=2007 & Grade=="5-8"], 
                          aes(x=Year, y=value)) +
  geom_line(aes(group=School, color=School)) + 
  geom_point(data=ISAT.plotdata[School=="KCCP" & Grade=="5-8"], 
             aes(x=Year, y=value, color=School), shape=18, size=3.5) +
  geom_point(data=ISAT.maxmin[Grade=="5-8"], aes(x=Year, y=value, color=grp)) + 
  facet_grid(Grade ~ variable) + 
  scale_color_manual(values = c("#E27425", #KAMS
                                "#439539", #CPS 
                                "#60A2D7", #KCCP
                                "#8D8685", #max
                                "#8D8685" #min
                                )
                     ) +
  scale_x_discrete(breaks=c("2007", "2009", "2011", "2013"))+
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x= element_text(size= 8,angle=35),
        strip.text = element_text(size=8),
        plot.margin = unit(c(0.5, 0, 0.5, 0.5), "lines")
  ) 




ISAT.58.Comp.tbl<-ggplot(ISAT.plotdata[Year>=2007 & Grade=="5-8"], 
                         aes(x=Year, y=y.label.pos)) + 
  geom_text(aes(label=round(value), color=School), size=3.5) + 
  facet_grid(Grade~variable) + 
  #theme_bw +
  theme(panel.grid.major = element_blank(), 
        #panel.border = element_blank(), 
        axis.text.y =  element_blank(),
        axis.ticks =  element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(), 
        axis.text.x= element_text(size= 8,angle=35),
        strip.text = element_text(size=8),
        #strip.background = element_blank(),
        legend.position="none",
        plot.margin = unit(c(0.5, 0.5, 0.5, -1), "lines")
  ) + 
  ylim(0.5, -0.5) +
  scale_color_manual(values = c("#E27425", #KAMS
                                "#439539", #CPS 
                                "#60A2D7", #KCCP
                                "#BCD631", #max
                                "#BCD631" #min
                                )
                     )






#Enrollment Data
#Enrollment - by grade
#FRM Data
#Ethnicity Data
#Special Ed Datai


setnames(Enrollment.table, "Enrollment", "#\nStudents")
setnames(Enrollment.table, "#\nStudents", "n")
Enroll.gtbl<-tableGrob(Enrollment.table,gpar.coretext=gpar(fontsize=9),gpar.coltext=gpar(fontsize=8), show.rownames=FALSE)


#Transfers out - by grade with reason
transfer.gtbl<-tableGrob(Xfer.table, show.rownames=FALSE, 
                         gpar.coretext=gpar(fontsize=8),
                         gpar.coltext=gpar(fontsize=8))

xferplot.1213<-data.table(xferplot.1213)

xferplot.1213[,School:=factor(School, 
                              levels=c("KAPS", "KAMS", "KCCP"), 
                              ordered=T)]

t10pct.df<-data.frame(School=c("KAMS","KAPS", "KCCP"), Month="May", Variable="10% Limit", Value=c(30, 28,7))

tly.df<-data.frame(School=c("KAMS", "KAPS"), Month="May", Variable="Last Year", Value=c(12,5))

transfer.plot<-ggplotGrob(ggplot(data=subset(xferplot.1213, Variable=="Ceiling"), aes(x=Month, y=Value)) + 
  geom_line(aes(group=Variable), color="#E27425") + 
  geom_line(data=subset(xferplot.1112, Variable!="Ceiling"), aes(x=Month, y=CumVal, group=Variable), color="#17345B") + 
  geom_bar(data=subset(xferplot.1213, Variable!="Ceiling"), aes(x=Month, y=Value), fill="#439539", width=.5) + 
  facet_grid(School~., scale="free_y") +
  geom_text(data=t10pct.df, aes(label=Variable), color="#E27425", size=3.7) + 
  geom_text(data=tly.df, aes(label=Variable), color="#17345B", size=3.7))



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


