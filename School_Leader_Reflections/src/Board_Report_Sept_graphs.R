map.cps[,Perc_Above_50th:=NULL]

map.kipp<-copy(MAP.KIPP.Network.1213)
map.kipp[,c("Col", "Mag_Growth"):=NULL]


map.combined<-rbind(map.kipp,map.cps)
map.combined[Sub_Test_Name=="Math", Sub_Test_Name:="Mathematics"]



map.kippchi.cps<-copy(map.combined[(Region_Name=="KIPP Chicago" 
                                    |Region_Name=="Charter") 
                                   & Growth.Season=="Fall to Spring" 
                                   & Growth_Grade_Level %in% c(2,5:8) 
                                   ])





#### MAP Growth Reading vs CPS Charters ####
map.kippchi.cps[,School:=reorder(factor(School_Display_Name), Perc_Growth), by=list(Growth_Grade_Level, Sub_Test_Name)]

ggplot(data=map.kippchi.cps[Growth_Grade_Level>=5 
                            & Sub_Test_Name=="Reading"]) + 
  geom_segment(aes(x=0, 
                   xend=Perc_Growth, 
                   y=School, 
                   yend=School,
                   color=School_Display_Name_Chi
  ), 
               size=2.5
  ) + 
  geom_text(data=map.kippchi.cps[Growth_Grade_Level>=5 
                                 & Sub_Test_Name=="Reading"
                                 & School_Display_Name_Chi!="CPS"],
            aes(x=Perc_Growth + 0.5,
                y=School,
                label=Perc_Growth,
                color=School_Display_Name_Chi),
            size=4,
            hjust=0) +
  facet_wrap( ~ Growth_Grade_Level, ncol=2) +
  scale_color_manual(values = c("#CFCCC1",  #All other KIPP
                                #"#C49A6C",  #KIPP National Average
                                #"#E27425",   #National Norm 
                                "#439539",  #KAMS
                                #"purple",  #KAPS
                                "#60A2D7"
  )  #KCCP)
  ) +
  theme(axis.text.y = element_blank(),
        #axis.text.x = element_text(size=fs-.5),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        #strip.text = element_text(size=fs-.5),
        legend.position="bottom")


#### MAP Growth Math vs CPS Charters ####
ggplot(data=map.kippchi.cps[Growth_Grade_Level>=5 
                            & Sub_Test_Name=="Mathematics"]) + 
  geom_segment(aes(x=0, 
                   xend=Perc_Growth, 
                   y=School, 
                   yend=School,
                   color=School_Display_Name_Chi
  ), 
               size=2.5
  ) + 
  geom_text(data=map.kippchi.cps[Growth_Grade_Level>=5 
                                 & Sub_Test_Name=="Mathematics"
                                 & School_Display_Name_Chi!="CPS"],
            aes(x=Perc_Growth + 0.5,
                y=School,
                label=Perc_Growth,
                color=School_Display_Name_Chi),
            size=4,
            hjust=0) +
  facet_wrap( ~ Growth_Grade_Level, ncol=2) +
  scale_color_manual(values = c("#CFCCC1",  #All other KIPP
                                #"#C49A6C",  #KIPP National Average
                                #"#E27425",   #National Norm 
                                "#439539",  #KAMS
                                #"purple",  #KAPS
                                "#60A2D7"
  )  #KCCP)
  ) +
  theme(axis.text.y = element_blank(),
        #axis.text.x = element_text(size=fs-.5),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        #strip.text = element_text(size=fs-.5),
        legend.position="bottom")






#### MAP Attainment Reading vs CPS Charters Middle  Fall Ranking####
map.kippchi.cps[,School:=reorder(factor(School_Display_Name), Start_RIT), by=list(Growth_Grade_Level, Sub_Test_Name)]


ggplot(data=map.kippchi.cps[Growth_Grade_Level>=5 
                            & Sub_Test_Name=="Reading"]) + 
  geom_segment(aes(x=Start_RIT, 
                   xend=End_RIT, 
                   y=School, 
                   yend=School,
                   color=School_Display_Name_Chi
  ), 
               size=2.5,
               arrow=arrow(length=unit(2, "points"), type="closed")
  ) + 
  geom_text(data=map.kippchi.cps[Growth_Grade_Level>=5 
                                 & Sub_Test_Name=="Reading"
                                 & School_Display_Name_Chi!="CPS"],
            aes(x=End_RIT + 0.5,
                y=School,
                label=End_RIT,
                color=School_Display_Name_Chi),
            size=4,
            hjust=0) +
  geom_text(data=map.kippchi.cps[Growth_Grade_Level>=5 
                                 & Sub_Test_Name=="Reading"
                                 & School_Display_Name_Chi!="CPS"],
            aes(x=Start_RIT - 0.5,
                y=School,
                label=Start_RIT,
                color=School_Display_Name_Chi),
            size=4,
            hjust=1) +
  facet_wrap( ~ Growth_Grade_Level, ncol=2) +
  scale_color_manual(values = c("#CFCCC1",  #All other KIPP
                                #"#C49A6C",  #KIPP National Average
                                #"#E27425",   #National Norm 
                                "#439539",  #KAMS
                                #"purple",  #KAPS
                                "#60A2D7"
  )  #KCCP)
  ) +
  theme(axis.text.y = element_blank(),
        #axis.text.x = element_text(size=fs-.5),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        #strip.text = element_text(size=fs-.5),
        legend.position="bottom")

#### MAP Attainment Mathematics vs CPS Charters Middle Fall Ranking ####



ggplot(data=map.kippchi.cps[Growth_Grade_Level>=5 
                            & Sub_Test_Name=="Mathematics"]) + 
  geom_segment(aes(x=Start_RIT, 
                   xend=End_RIT, 
                   y=School, 
                   yend=School,
                   color=School_Display_Name_Chi
  ), 
               size=2.5,
               arrow=arrow(length=unit(2, "points"), type="closed")
  ) + 
  geom_text(data=map.kippchi.cps[Growth_Grade_Level>=5 
                                 & Sub_Test_Name=="Mathematics"
                                 & School_Display_Name_Chi!="CPS"],
            aes(x=End_RIT + 0.5,
                y=School,
                label=End_RIT,
                color=School_Display_Name_Chi),
            size=4,
            hjust=0) +
  geom_text(data=map.kippchi.cps[Growth_Grade_Level>=5 
                                 & Sub_Test_Name=="Mathematics"
                                 & School_Display_Name_Chi!="CPS"],
            aes(x=Start_RIT - 0.5,
                y=School,
                label=Start_RIT,
                color=School_Display_Name_Chi),
            size=4,
            hjust=1) +
  facet_wrap( ~ Growth_Grade_Level, ncol=2) +
  scale_color_manual(values = c("#CFCCC1",  #All other KIPP
                                #"#C49A6C",  #KIPP National Average
                                #"#E27425",   #National Norm 
                                "#439539",  #KAMS
                                #"purple",  #KAPS
                                "#60A2D7"
  )  #KCCP)
  ) +
  theme(axis.text.y = element_blank(),
        #axis.text.x = element_text(size=fs-.5),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        #strip.text = element_text(size=fs-.5),
        legend.position="bottom")


#### MAP Attainment Reading vs CPS Charters Middle Spring Ranking####
map.kippchi.cps[,School:=reorder(factor(School_Display_Name), End_RIT), by=list(Growth_Grade_Level, Sub_Test_Name)]


ggplot(data=map.kippchi.cps[Growth_Grade_Level>=5 
                            & Sub_Test_Name=="Reading"]) + 
  geom_segment(aes(x=Start_RIT, 
                   xend=End_RIT, 
                   y=School, 
                   yend=School,
                   color=School_Display_Name_Chi
  ), 
               size=2.5,
               arrow=arrow(length=unit(2, "points"), type="closed")
  ) + 
  geom_text(data=map.kippchi.cps[Growth_Grade_Level>=5 
                                 & Sub_Test_Name=="Reading"
                                 & School_Display_Name_Chi!="CPS"],
            aes(x=End_RIT + 0.5,
                y=School,
                label=End_RIT,
                color=School_Display_Name_Chi),
            size=4,
            hjust=0) +
  geom_text(data=map.kippchi.cps[Growth_Grade_Level>=5 
                                 & Sub_Test_Name=="Reading"
                                 & School_Display_Name_Chi!="CPS"],
            aes(x=Start_RIT - 0.5,
                y=School,
                label=Start_RIT,
                color=School_Display_Name_Chi),
            size=4,
            hjust=1) +
  facet_wrap( ~ Growth_Grade_Level, ncol=2) +
  scale_color_manual(values = c("#CFCCC1",  #All other KIPP
                                #"#C49A6C",  #KIPP National Average
                                #"#E27425",   #National Norm 
                                "#439539",  #KAMS
                                #"purple",  #KAPS
                                "#60A2D7"
  )  #KCCP)
  ) +
  theme(axis.text.y = element_blank(),
        #axis.text.x = element_text(size=fs-.5),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        #strip.text = element_text(size=fs-.5),
        legend.position="bottom")

#### MAP Attainment Mathematics vs CPS Charters Middle Spring Ranking ####



ggplot(data=map.kippchi.cps[Growth_Grade_Level>=5 
                            & Sub_Test_Name=="Mathematics"]) + 
  geom_segment(aes(x=Start_RIT, 
                   xend=End_RIT, 
                   y=School, 
                   yend=School,
                   color=School_Display_Name_Chi
  ), 
               size=2.5,
               arrow=arrow(length=unit(2, "points"), type="closed")
  ) + 
  geom_text(data=map.kippchi.cps[Growth_Grade_Level>=5 
                                 & Sub_Test_Name=="Mathematics"
                                 & School_Display_Name_Chi!="CPS"],
            aes(x=End_RIT + 0.5,
                y=School,
                label=End_RIT,
                color=School_Display_Name_Chi),
            size=4,
            hjust=0) +
  geom_text(data=map.kippchi.cps[Growth_Grade_Level>=5 
                                 & Sub_Test_Name=="Mathematics"
                                 & School_Display_Name_Chi!="CPS"],
            aes(x=Start_RIT - 0.5,
                y=School,
                label=Start_RIT,
                color=School_Display_Name_Chi),
            size=4,
            hjust=1) +
  facet_wrap( ~ Growth_Grade_Level, ncol=2) +
  scale_color_manual(values = c("#CFCCC1",  #All other KIPP
                                #"#C49A6C",  #KIPP National Average
                                #"#E27425",   #National Norm 
                                "#439539",  #KAMS
                                #"purple",  #KAPS
                                "#60A2D7"
  )  #KCCP)
  ) +
  theme(axis.text.y = element_blank(),
        #axis.text.x = element_text(size=fs-.5),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        #strip.text = element_text(size=fs-.5),
        legend.position="bottom")



#### MAP Attainment Lines Fall to Spring #####

ggplot(data=map.kippchi.cps[Growth_Grade_Level>=5 
                            #& Sub_Test_Name=="Mathematics"
                            ]) + 
  geom_segment(aes(x=0, 
                   xend=1, 
                   y=Start_RIT, 
                   yend=End_RIT,
                   color=School_Display_Name_Chi
  ), 
               size=1.5, alpha=.4
  ) + 
  geom_segment(data=map.kippchi.cps[Growth_Grade_Level>=5 
                                    & School_Display_Name_Chi!="CPS"
                                    ],
               aes(x=0, 
                   xend=1, 
                   y=Start_RIT, 
                   yend=End_RIT,
                   color=School_Display_Name_Chi
               ), 
               size=2) + 
  geom_text(data=map.kippchi.cps[Growth_Grade_Level>=5 
                                 #& Sub_Test_Name=="Mathematics"
                                 & School_Display_Name_Chi!="CPS"],
            aes(x=1,
                y=End_RIT,
                label=End_RIT,
                color=School_Display_Name_Chi),
            size=4,
            hjust=0) +
  geom_text(data=map.kippchi.cps[Growth_Grade_Level>=5 
                                 #& Sub_Test_Name=="Mathematics"
                                 & School_Display_Name_Chi!="CPS"],
            aes(x=0,
                y=Start_RIT,
                label=Start_RIT,
                color=School_Display_Name_Chi),
            size=4,
            hjust=1) +
  #facet_wrap( ~ Growth_Grade_Level, ncol=2) +
  facet_grid(Sub_Test_Name ~ Growth_Grade_Level) +
  scale_color_manual(values = c("#CFCCC1",  #All other KIPP
                                #"#C49A6C",  #KIPP National Average
                                #"#E27425",   #National Norm 
                                "#439539",  #KAMS
                                #"purple",  #KAPS
                                "#60A2D7"
  )  #KCCP)
  ) +
  theme(axis.text.y = element_blank(),
        #axis.text.x = element_text(size=fs-.5),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        #strip.text = element_text(size=fs-.5),
        legend.position="bottom") + xlim(-.2,1.2)

############################



#### MAP KIPP Foundation data ####

# We only want Fall to Spring for these comparisons 
map.kipp<-copy(MAP.KIPP.Network.1213[Growth_Grade_Level %in% kc.grades
                                     & Growth.Season=="Fall to Spring"])

map.kipp[,School:=reorder(factor(School_Display_Name), Perc_Growth), by=list(Growth_Grade_Level, Sub_Test_Name)]

map.kipp[,School_Display_Name_Chi:=
           factor(School_Display_Name_Chi, 
                  levels=c("KIPP", 
                           "KIPP Network",
                           "National Norm",
                           "KIPP Ascend Middle School",
                           "KIPP Ascend Primary School",
                           "KIPP Create Middle School"))]


#### MAP Growth Reading vs KIPP Network Middle ####
ggplot(data=map.kipp[Growth_Grade_Level>=5 
                     & Sub_Test_Name=="Reading"]) + 
  geom_segment(aes(x=0, 
                   xend=Perc_Growth, 
                   y=School, 
                   yend=School,
                   color=School_Display_Name_Chi
  ), 
               size=1
  ) + 
  geom_text(data=map.kipp[Growth_Grade_Level>=5 
                          & Sub_Test_Name=="Reading"
                          & School_Display_Name_Chi!="KIPP"],
            aes(x=Perc_Growth + 0.5,
                y=School,
                label=Perc_Growth,
                color=School_Display_Name_Chi),
            size=4,
            hjust=0) +
  facet_wrap( ~ Growth_Grade_Level, ncol=2) +
  scale_color_manual(values = c("#CFCCC1",  #All other KIPP
                                "#C49A6C",  #KIPP National Average
                                "#E27425",   #National Norm 
                                "#439539",  #KAMS
                                #"purple",  #KAPS
                                "#60A2D7"
  )  #KCCP)
  ) +
  theme(axis.text.y = element_blank(),
        #axis.text.x = element_text(size=fs-.5),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        #strip.text = element_text(size=fs-.5),
        legend.position="bottom")





#map.all[Sub_Test_Name=="Mathematics", Sub_Test_Name:="Math"]

#### MAP Growth Math vs KIPP Network Middle ####
ggplot(data=map.kipp[Growth_Grade_Level>=5 
                     & Sub_Test_Name=="Mathematics"]) + 
  geom_segment(aes(x=0, 
                   xend=Perc_Growth, 
                   y=School, 
                   yend=School,
                   color=School_Display_Name_Chi
  ), 
               size=1
  ) + 
  geom_text(data=map.kipp[Growth_Grade_Level>=5 
                          & Sub_Test_Name=="Mathematics"
                          & School_Display_Name_Chi!="KIPP"],
            aes(x=Perc_Growth + 0.5,
                y=School,
                label=Perc_Growth,
                color=School_Display_Name_Chi),
            size=4,
            hjust=0) +
  facet_wrap( ~ Growth_Grade_Level, ncol=2) +
  scale_color_manual(values = c("#CFCCC1",  #All other KIPP
                                "#C49A6C",  #KIPP National Average
                                "#E27425",   #National Norm 
                                "#439539",  #KAMS
                                #"purple",  #KAPS
                                "#60A2D7"
  )  #KCCP)
  ) +
  theme(axis.text.y = element_blank(),
        #axis.text.x = element_text(size=fs-.5),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        #strip.text = element_text(size=fs-.5),
        legend.position="bottom")



#### MAP Growth Reading vs KIPP Network Primary ####
ggplot(data=map.kipp[Growth_Grade_Level<5 
                     & Sub_Test_Name=="Reading"]) + 
  geom_segment(aes(x=0, 
                   xend=Perc_Growth, 
                   y=School, 
                   yend=School,
                   color=School_Display_Name_Chi
  ), 
               size=2
  ) + 
  geom_text(data=map.kipp[Growth_Grade_Level<5 
                          & Sub_Test_Name=="Reading"
                          & School_Display_Name_Chi!="KIPP"],
            aes(x=Perc_Growth + 0.5,
                y=School,
                label=Perc_Growth,
                color=School_Display_Name_Chi),
            size=4,
            hjust=0) +
  facet_wrap( ~ Growth_Grade_Level, ncol=2) +
  scale_color_manual(values = c("#CFCCC1",  #All other KIPP
                                "#C49A6C",  #KIPP National Average
                                "#E27425",   #National Norm 
                                "#439539",  #KAMS
                                #"purple",  #KAPS
                                "#60A2D7"
  )  #KCCP)
  ) +
  theme(axis.text.y = element_blank(),
        #axis.text.x = element_text(size=fs-.5),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        #strip.text = element_text(size=fs-.5),
        legend.position="bottom")






#### MAP Growth Math vs KIPP Network Middle ####
ggplot(data=map.kipp[Growth_Grade_Level<5 
                     & Sub_Test_Name=="Mathematics"]) + 
  geom_segment(aes(x=0, 
                   xend=Perc_Growth, 
                   y=School, 
                   yend=School,
                   color=School_Display_Name_Chi
  ), 
               size=2
  ) + 
  geom_text(data=map.kipp[Growth_Grade_Level<5 
                          & Sub_Test_Name=="Mathematics"
                          & School_Display_Name_Chi!="KIPP"],
            aes(x=Perc_Growth + 0.5,
                y=School,
                label=Perc_Growth,
                color=School_Display_Name_Chi),
            size=4,
            hjust=0) +
  facet_wrap( ~ Growth_Grade_Level, ncol=2) +
  scale_color_manual(values = c("#CFCCC1",  #All other KIPP
                                "#C49A6C",  #KIPP National Average
                                "#E27425",   #National Norm 
                                "#439539",  #KAMS
                                #"purple",  #KAPS
                                "#60A2D7"
  )  #KCCP)
  ) +
  theme(axis.text.y = element_blank(),
        #axis.text.x = element_text(size=fs-.5),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        #strip.text = element_text(size=fs-.5),
        legend.position="bottom")



#### Working Stuff ####
map.cps.attainment<-MAP.CPS.Network.1213[Grade=="All Grades Combined", 
                                         list(Pct_50th=sum(as.numeric(AGG_Pct_At_or_Above_Natl_Avg_Spring2013)
                                                           *as.numeric(TotalTested_Spring2013))
                                              /sum(as.numeric(TotalTested_Spring2013))
                                         ), 
                                         by=list(SCHLTYPE1, NWEA_Subject)]

map.cps.attainment<-MAP.CPS.Network.1213[Grade %in% 
                                           c("2nd Grade", 
                                             "5th Grade", 
                                             "6th Grade", 
                                             "7th Grade", 
                                             "8th Grade"), 
                                         list(Pct_50th=sum(
                                           as.numeric(
                                             AGG_Pct_At_or_Above_Natl_Avg_Spring2013)
                                           *as.numeric(TotalTested_Spring2013), 
                                           na.rm=TRUE)
                                              /sum(as.numeric(TotalTested_Spring2013), 
                                                   na.rm=TRUE)
                                         ),
                                         by=list(SCHLTYPE1, NWEA_Subject)]


map.cps.attainment[NWEA_Subject=="Math", NWEA_Subject:="Mathematics"]


map.chi.attainment<-map.summary.1213[as.numeric(Fall12_Grade)>2 & Subject!="General Science", list(SCHLTYPE1="KIPP Chicago", NWEA_Subject=Subject, Pct_50th=sum(tot.50th.spring)/sum(tot.students)), by=Subject]

map.chi.attainment[,Subject:=NULL]

map.cpschi.attainment<- rbind(map.cps.attainment, map.chi.attainment)

map.cpschi.attainment[SCHLTYPE1 %in% c("Selective","Selective Mag","Gifted", "Classical"), School_Type:="Selective"]
map.cpschi.attainment[SCHLTYPE1 %in% c("Charter", "Controact", "Non-sel Mag", "Traditional Nbrhd", "Contract"), School_Type:="Non-Selective"]
map.cpschi.attainment[SCHLTYPE1 == "Citywide avg.", School_Type:="CPS Average"]
map.cpschi.attainment[SCHLTYPE1 == "KIPP Chicago", School_Type:="KIPP Chicago"]



map.cpschi.attainment[,School_Order:=reorder(factor(SCHLTYPE1, labels=as.character(SCHLTYPE1)), Pct_50th), 
                      by=NWEA_Subject]

map.cpschi.attainment[,test:=factor(SCHLTYPE1, levels=reorder(SCHLTYPE1, Pct_50th)), 
                      by=NWEA_Subject]

map.


#setnames(map.cpschi.attainment, c("School_Type", "Subject", "Percent_Above _0th Percentile"))

g1<-ggplot(map.cpschi.attainment[NWEA_Subject=="Mathematics"], 
           aes(x=reorder(SCHLTYPE1, Pct_50th) ,y=Pct_50th, fill=School_Type)) +
  geom_bar(stat="identity") + 
  # facet_grid(~NWEA_Subject) +
  scale_fill_manual(values = c("#C49A6C",  #CPS Average
                               "#439539",  #KAMS
                               "#cecece",  #Non Selective 
                               "#CFCCC1") #Selective                       
  ) +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(hjust=1, angle=45),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        #strip.text = element_text(size=fs-.5),
        legend.position="none")

g2<-ggplot(map.cpschi.attainment[NWEA_Subject=="Reading"], 
           aes(x=reorder(SCHLTYPE1, Pct_50th) ,y=Pct_50th, fill=School_Type)) +
  geom_bar(stat="identity") + 
  # facet_grid(~NWEA_Subject) +
  scale_fill_manual(values = c("#C49A6C",  #CPS Average
                               "#439539",  #KAMS
                               "#cecece",  #Non Selective 
                               "#CFCCC1") #Selective                       
  ) +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(hjust=1, angle=45),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        #strip.text = element_text(size=fs-.5),
        legend.position="none",
        panel.title=element_text(lineheight=.8, gp=gpar(fill="grey"))) +
  +ggtitle("Reading")


grid.arrange(g1+ggtitle("Mathematics"), g2+ggtitle("Reading"), ncol=2)