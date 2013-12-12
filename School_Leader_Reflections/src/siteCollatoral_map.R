map.plot<-copy(map.combined[Growth.Season=="Fall to Spring" 
             & Growth_Grade_Level %in% c(5:8) 
             & School_Display_Name_Chi!="KIPP"])

map.plot[,Type:=Region_Name]

map.plot[Type %in% c("Selective Mag", "Selective", "Gifted", "Classical"),Type:="Selective Enrollment"]

map.plot[Type %in% c("Contract", "Non-sel Mag", "Traditional Nbrhd"), Type:="Non-Selective Enrollment"]


#### % Growth Stacked dot plot with jitter ####
ggplot(map.plot[Type!="Citywide avg."&Type!="No Region"], 
       aes(x=0, 
           y=Perc_Growth
           )
       ) + 
  geom_point(aes(color=Type, size=rev(Type)), 
             size=4,
             alpha=.5,
             position=position_jitter(w=.4,h=.05)) + 
  geom_point(data=map.plot[Region_Name=="KIPP Chicago"], 
             aes(x=0, 
                 y=Perc_Growth, 
                 color=Type
             ), 
             size=8,
             shape=17
             ) +
  geom_hline(data=map.plot[Region_Name=="Citywide avg."], 
             aes(yintercept=Perc_Growth), 
             color="red") +
  facet_grid(Sub_Test_Name~Growth_Grade_Level) + xlim(-1,1) + 
  scale_color_manual(values=c("#F7941E", #charters 
                              "#F7941E",
                              "#17345B", #KIPP
                              "#60A2D7",
                              "purple", #SE
                              "#CFCCC1"  #NSE  
                              )
                     ) + 
  guides(colour=guide_legend(override.aes=list(shape=15, alpha=1,size=6)))


#### % End_RIT - Start_RIT Stacked dot plot with jitter ####
ggplot(map.plot[Type!="Citywide avg."&Type!="No Region"], 
       aes(x=0, 
           y=End_RIT - Start_RIT
       )
) + 
  geom_point(aes(color=Type, size=rev(Type)), 
             size=4,
             alpha=.5,
             position=position_jitter(w=.4,h=.05)) + 
  geom_point(data=map.plot[Region_Name=="KIPP Chicago"], 
             aes(x=0, 
                 y=End_RIT - Start_RIT, 
                 color=Type
             ), 
             size=8,
             shape=17
  ) +
  geom_hline(
             aes(yintercept=mean(End_RIT-Start_RIT, na.rm=TRUE)), 
             color="red") +
  facet_grid(Sub_Test_Name~Growth_Grade_Level) + xlim(-1,1) + 
  scale_color_manual(values=c("#F7941E", #charters 
                              "#17345B", #KIPP
                              "purple", #SE
                              "#CFCCC1"  #NSE  
  )
  ) + 
  guides(colour=guide_legend(override.aes=list(shape=15, alpha=1,size=6)))


#### End RIT - Start RIT by % Growth ####
ggplot(map.plot[Type!="Citywide avg."|Type!="No Region"], 
       aes(x=End_RIT-Start_RIT, 
           y=Perc_Growth
       )
) + 
  geom_point(aes(color=Type, size=rev(Type)), 
             size=4,
             alpha=.5,
             position=position_jitter(w=.4,h=.05)) + 
  geom_hline(data=map.plot[Region_Name=="Citywide avg."], 
             aes(yintercept=Perc_Growth), 
             color="red") +
  geom_vline(data=map.plot[,list(avg=mean(End_RIT-Start_RIT, na.omit=TRUE)), by=list(Growth_Grade_Level, Sub_Test_Name)], 
             aes(xintercept=avg), 
             color="red") +
  
  geom_point(data=map.plot[Region_Name=="KIPP Chicago"], 
             aes(x=End_RIT-Start_RIT, 
                 y=Perc_Growth, 
                 color=Type
             ), 
             size=8,
             shape=17
  ) +
  facet_grid(Sub_Test_Name~Growth_Grade_Level) + 
  scale_color_manual(values=c("#F7941E", #charters 
                              "#F7941E",
                              "#17345B", #KIPP
                              "#60A2D7",
                              "purple", #SE
                              "#CFCCC1"  #NSE  
  )
  ) + 
  guides(colour=guide_legend(override.aes=list(shape=15, alpha=1,size=6)))

#### All Grades Combined ####
map.kipp<-copy(MAP.KIPP.Network.1213)

map.combined.grades.kippchi<-copy(
  map.kipp[Region_Name=="KIPP Chicago" & Growth.Season=="Fall to Spring",
           list(
                Start_RIT=sum((N*Start_RIT))/sum(N),
                End_RIT=sum((N*End_RIT))/sum(N),
                Perc_Growth=sum((N*Perc_Growth))/sum(N),  
                N=sum(N), 
                School_Display_Name=as.character(Region_Name),
                School_Display_Name_Chi=as.character(Region_Name),
                Growth_Grade_Level="All Grades Combined"), 
           by=list(Sub_Test_Name, Growth.Season, Region_Name)]
  )

map.combined.grades.kippchi[,Growth.Season:=as.character(Growth.Season)]

map.combined.grades[,Perc_Above_50th:=NULL]

map.combined.grades[Sub_Test_Name=="Math", Sub_Test_Name:="Mathematics"]

map.plot.combined<-rbind(map.combined.grades, map.combined.grades.kippchi)

map.plot.combined[,Type:=Region_Name]

map.plot.combined[Type %in% c("Selective Mag", "Selective", "Gifted", "Classical"),Type:="Selective Enrollment"]

map.plot.combined[Type %in% c("Contract", "Non-sel Mag", "Traditional Nbrhd"), Type:="Non-Selective Enrollment"]

#Add Percentiles of % meeting/exc
map.plot.combined[School_Display_Name!="CPS",Pctl:=perc.rank(Perc_Growth), by=list(Sub_Test_Name, Growth_Grade_Level)]

#And now quartile

map.plot.combined[,Qrtl:=NULL]
map.plot.combined[School_Display_Name!="CPS" & Pctl<25 ,Qrtl:="1"]
map.plot.combined[School_Display_Name!="CPS" & (Pctl>=25 & Pctl<50) ,Qrtl:="2"]
map.plot.combined[School_Display_Name!="CPS" & (Pctl>=50 & Pctl<75) ,Qrtl:="3"]
map.plot.combined[School_Display_Name!="CPS" & Pctl>=75 ,Qrtl:="4"]



map.plot.combined[Type=="KIPP Chicago", Qrtl:="KIPP Chicago"]

#Reverse Type ordering
map.plot.combined[,Type:=factor(Type, levels=rev(levels(Type)))]

#### Comparison school indicators ####
#Get comparison schools 
comp_schools_kams<-isat_comparison_student_list_400044_20130913[,unique(aaschlname)]
comp_schools_kccp<-isat_comparison_student_list_400146_20130913[,unique(aaschlname)]

comp_schools_kams<-data.table(School_Display_Name=comp_schools_kams, 
                              comp_kams=rep(TRUE,length(comp_schools_kams)))

comp_schools_kccp<-data.table(School_Display_Name=comp_schools_kccp, 
                              comp_kccp=rep(TRUE,length(comp_schools_kccp)))

setkey(comp_schools_kams, School_Display_Name)
setkey(comp_schools_kccp, School_Display_Name)

setkey(map.plot.combined, School_Display_Name)

map.plot.combined<-comp_schools_kams[map.plot.combined]
map.plot.combined[is.na(comp_kams), comp_kams:=FALSE]

map.plot.combined<-comp_schools_kccp[map.plot.combined]
map.plot.combined[is.na(comp_kccp), comp_kccp:=FALSE]


#### ALL GRADES COMBINED % Growth Stacked dot plot with jitter ####
ggplot(map.plot.combined[Type!="Citywide avg."&Type!="No Region"], 
       aes(x=0, 
           y=Perc_Growth
       )
) + 
  geom_point(aes(color=Type, size=rev(Type)), 
             size=4,
             alpha=.5,
             position=position_jitter(w=.4,h=.05)) + 
  geom_point(data=map.plot.combined[Region_Name=="KIPP Chicago"], 
             aes(x=0, 
                 y=Perc_Growth, 
                 color=Type
             ), 
             size=8,
             shape=17
  ) +
  geom_hline(data=map.plot.combined[Region_Name=="Citywide avg."], 
             aes(yintercept=Perc_Growth), 
             color="red") +
  facet_grid(Sub_Test_Name~.) + xlim(-1,1) + 
  scale_color_manual(values=c("#F7941E", #charters 
                              "#17345B", #KIPP
                              "purple", #SE
                              "#CFCCC1"  #NSE  
  )
  ) + 
  guides(colour=guide_legend(override.aes=list(shape=15, alpha=1,size=6))) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_blank())


#### Dot plot binned by Perc_Growth, fill=percentile of Perc_Growht, color=Type
ggplot(map.plot.combined[School_Display_Name!="CPS" & !is.na(Perc_Growth)]) +
  geom_dotplot(aes(x=Perc_Growth, fill=as.factor(Qrtl), color=Type), binpositions="all",method="histodot", stackgroups=T, binwidth=1) + 
  facet_grid(Growth_Grade_Level ~ Sub_Test_Name) +
  scale_color_manual(values=c("#CFCCC1",  #NSE
                              "purple", #SE
                              "#BCD631", #KIPP
                              "#F7941E" #charters 
                              )
                     ) + 
  scale_fill_manual(values = c("#f4efeb",
                               "#cfccc1",
                               "#c3bfbc",
                               "darkgray",
                               "#BCD631")
                    )

#### Dot plot binned by Perc_Growth, fill=percentile of Perc_Growht, color=Type
ggplot(map.plot.combined[School_Display_Name!="CPS" & !is.na(Perc_Growth) 
                         & (comp_kams==T|Type=="KIPP Chicago")]) +
  geom_dotplot(aes(x=1, y=Perc_Growth, 
                   fill=as.factor(Qrtl),
                   color=Type
                   ), 
               binaxis="y", 
               stackdir="center", 
               method="histodot", 
               binwidth=1, 
               binpositions="all",
               stackgroups=F) + 
  facet_grid(Growth_Grade_Level ~ Sub_Test_Name) +
  scale_color_manual(values=c("#CFCCC1",  #NSE
                              "purple", #SE
                              "#BCD631", #KIPP
                              "#F7941E" #charters 
  )
  ) + 
  scale_fill_manual(values = c("#f4efeb",
                               "#cfccc1",
                               "#c3bfbc",
                               "darkgray",
                               "#BCD631")
  )

