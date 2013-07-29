
kc.grades<-c(0:2,5:8)
kc.schools<-c("KIPP Create Middle School", "KIPP Ascend Middle School", "KIPP Ascend Primary School")

# bars
ggplot(MAP.KIPP.Network.1213[Growth.Season=="Fall to Spring" & 
                               Growth_Grade_Level==5 &
                               Sub_Test_Name=="Reading", 
                             ]
) + 
  geom_bar(aes(x=reorder(School_Display_Name, Perc_Growth), 
               y=Perc_Growth,
               color=Col,
               fill=Col), 
           stat='identity',
           width=.8) +
  coord_flip() +
  scale_color_manual(values = c("#CFCCC1", "#439539")) + 
  scale_fill_manual(values = c("#CFCCC1", "#439539")) + 
  scale_x_discrete(breaks=MAP.KIPP.Network.1213[,School_Display_Name_Chi]) 
#theme(axis.text.y = element_text(size=5))

#points
ggplot(MAP.KIPP.Network.1213[Growth.Season=="Fall to Spring" & 
                               Growth_Grade_Level==5 &
                               Sub_Test_Name=="Reading", 
                             ]
) + 
  geom_point(aes(x=reorder(School_Display_Name, Perc_Growth), 
                 y=Perc_Growth,
                 color=School_Display_Name_Chi
                 ,size=School_Display_Name_Chi )
  ) +
  coord_flip() +
  scale_color_manual(values = c("#439539",  #KAMS
                                "#60A2D7",  #KCCP
                                "#CFCCC1"   #All other KIPP
  )
  ) + 
  theme(axis.text.y = element_blank(),
        legend.position="none")

# Another Try at points


# Subset and reorder by school display name
dt<-copy(MAP.KIPP.Network.1213[Growth.Season=="Fall to Spring" & 
                                 Growth_Grade_Level==5 &
                                 Sub_Test_Name=="Reading", 
                               School:=
                                 reorder(School_Display_Name, Perc_Growth)])

dt.all<-copy(MAP.KIPP.Network.1213[Growth.Season=="Fall to Spring" &
                                     Growth_Grade_Level %in% kc.grades])

dt.all[,School:=reorder(factor(School_Display_Name), Perc_Growth), by=list(Growth_Grade_Level, Sub_Test_Name)]


ggplot(dt, aes(x=School, y=Perc_Growth)) +
  geom_point() +
  geom_point(data=dt[School=="KIPP Ascend Middle School"], 
             aes(x=School, 
                 y=Perc_Growth), 
             color="#439539",
             size=5) +
  geom_point(data=dt[School=="KIPP Create Middle School"], 
             aes(x=School, 
                 y=Perc_Growth), 
             color="#60A2D7",
             size=5) +
  coord_flip() +
  theme(axis.text.y = element_blank(),
        legend.position="none")



##### Points
ggplot(dt.all, aes(x=School, y=Perc_Growth)) +
  geom_point(aes(color=School_Display_Name_Chi,
                 size=School_Display_Name_Chi)) +
  coord_flip() +
  facet_grid(Sub_Test_Name ~ Growth_Grade_Level) +
  scale_color_manual(values = c("#439539",  #KAMS
                                "purple",  #KAPS
                                "#60A2D7",  #KCCP
                                "#CFCCC1"   #All other KIPP
  ) 
  ) +
  scale_size_manual(values = c(4,4,4,1.5)) +
  theme(axis.text.y = element_blank(),
        legend.position="none")

##### bar again
ggplot(dt.all, aes(x=School, y=Perc_Growth)) +
  geom_bar(aes(color=School_Display_Name_Chi
  ), stat='identity') +
  coord_flip() +
  facet_grid(Sub_Test_Name ~ Growth_Grade_Level) +
  scale_color_manual(values = c("#439539",  #KAMS
                                "purple",  #KAPS
                                "#60A2D7",  #KCCP
                                "#CFCCC1"   #All other KIPP
  ) 
  ) +
  scale_size_manual(values = c(4,4,4,2)) +
  theme(axis.text.y = element_blank(),
        legend.position="none")