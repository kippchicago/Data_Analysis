# May BOD meeting script

# Need HSR dots with top quartile of quartile's named, others' anonymized



require(ProjectTemplate)
load.project()

todays_date <-format(today(), "%y%m%d")

# Regional Top Quartile Performers ####

# aggreate reginal data to unique instances of regional, role, parent.topic
hsr.reg<-copy(hsr.regional[,list(Count=.N), by=list(Region,
                                        Rank,
                                        Role, 
                                        Parent_Topic)][,Count:=NULL])
hsr.reg.role.tot<-hsr.reg[, .N, by=list(Region, Role)]

hsr.reg.sum<-hsr.reg.role.tot[,list(Role="Total", N=sum(N)), by=Region]

hsr.plot.data<-rbind(hsr.reg.role.tot, hsr.reg.sum)

hsr.plot.data[,Role:=factor(Role, 
                            levels=c("Students", 
                                     "Parent", 
                                     "Teachers", 
                                     "Staff",
                                     "School Leader",
                                     "Total"))]

hsr.plot.data[,Region:=factor(Region, 
                              levels=hsr.reg.sum$Region[order(hsr.reg.sum$N)])]



# dot plot ####
# need to do some munging to get the cartesian product of each Role's parent topics x
# each region.

# first, let's get a vector of all regions in the data
regions<-hsr.reg[,unique(Region)]

#now we need to get unique roles
roles<-hsr.reg[,unique(Role)]
roles

#and for each role we need unique parent topics
pt.list<-lapply(roles, 
               function(x) hsr.reg[Role==x, 
                                   list(Role=x, 
                                        Parent_Topic=unique(Parent_Topic
                                                            )
                                        )
                                   ]
               )

# combine lists into data.table
ptopics<-rbindlist(pt.list)

# collapse Role and Parent topic
ptopics[, RPT:=paste(Role, Parent_Topic, sep=".")]

# return cartesion product of region by RPT
region_topic<-data.table(expand.grid(Region=regions, RPT=ptopics$RPT))

#extrat roles and parent topics from rpt
region_topic[,Role:=gsub("(.+)\\.(.+)", "\\1", RPT)]
region_topic[,Parent_Topic:=gsub("(.+)\\.(.+)", "\\2", RPT)]


#reorder regions using total top quartile parent topics
region_topic[,Region:=factor(Region, 
                             levels=hsr.reg.sum$Region[order(hsr.reg.sum$N)])]
hsr.reg[,Region:=factor(Region, 
                              levels=hsr.reg.sum$Region[order(hsr.reg.sum$N)])]



hsr.plot.data<-data.table(left_join(hsr.reg, hsr.reg.sum, by=c("Region")))

hsr.plot.data[,Role.y:=NULL]
setnames(hsr.plot.data, "Role.x", "Role")

for (r in 1:length(regions)){
  hsr.plot.data[Region==regions[r] & N>=10,Region_Anon:=Region]
  hsr.plot.data[Region==regions[r] & N<10,Region_Anon:=paste("KIPP Region", LETTERS[r])]
}

hsr.plot.data[,Region_Anon_N:=paste(Region_Anon, "(", N, ")")]

regions_anons<-hsr.plot.data[,unique(Region_Anon_N)]

region_topic_anons<-data.table(expand.grid(Region_Anon_N=regions_anons, RPT=ptopics$RPT))

#extrat roles and parent topics from rpt
region_topic_anons[,Role:=gsub("(.+)\\.(.+)", "\\1", RPT)]
region_topic_anons[,Parent_Topic:=gsub("(.+)\\.(.+)", "\\2", RPT)]



dotsize<-.5


#ranked by total top quartile ####
anon_names_2<-anon_names %>%
  mutate(Region_Name=ifelse(grepl("ENC", Region_Name), 
                            "KIPP ENC", 
                            Region_Name
                            ),
         Region=Region_Name) %>%
  select(-Region_Name)




  region_anon_ranked<-hsr.plot.data[,list(Region_Anon_N, N)][order(N)][,unique(Region_Anon_N)]
  #reorder regions using total top quartile parent topics
  region_topic_anons[,Region_Anon_N:=factor(Region_Anon_N, 
                                            levels=region_anon_ranked)]
  hsr.plot.data[,Region_Anon_N:=factor(Region_Anon_N, 
                                       levels=region_anon_ranked)]
  
  dot.r<-ggplot(hsr.plot.data,
              aes(x=Parent_Topic, y=Region_Anon_N)) + 
    geom_dotplot(data=region_topic_anons, 
                 fill="white", 
                 color="black", 
                 binaxis = "y", 
                 dotsize=dotsize,
    ) + 
    geom_dotplot(binaxis = "y", 
                 dotsize=dotsize,
                 fill="#17345B",
                 color="#17345B") +     
    geom_dotplot(data=hsr.plot.data[Region=="KIPP Chicago"],
                 binaxis = "y", 
                 dotsize=dotsize,
                 color="orange",
                 fill="orange") +   
    facet_grid(.~Role, 
               scales = "free_x") +
    theme_bw() + 
    theme(axis.text.x=element_text(angle=90)) +
    xlab("Topic") + 
    ylab("Region (Total Top Performer Topics in parenthesis)") +
    ggtitle("HSR Regional Top Performers\nCount of Top Quartile Topics by Shareholders")
  
dot.r
  
  pdf("graphs/BOD_hsr_dots_ranked.pdf", width=8, height=10.5)
   dot.r
  dev.off()

# Proper Dot plot for Top Perfermers
hsr.plot.data.2<-hsr.plot.data %>%
  select(-Region_Anon) %>%
  mutate(Region=as.character(Region)) %>% 
  left_join(anon_names_2, by="Region") 
  


hsr.plot.N.data<-hsr.plot.data.2 %>% 
  select(Region, N, Region_Anon) %>% 
  unique %>%
  mutate(Region=ifelse(rank(-N)>=7, Region_Anon, Region))

regions_anon_ranked<-hsr.plot.N.data %>%
  arrange(N) %>% select(Region) 

hsr.plot.N.data<-hsr.plot.N.data %>%
  mutate(Region=factor(Region, 
                       levels=regions_anon_ranked$Region
                       )
         )

hsr.top.2<-ggplot(hsr.plot.N.data, aes(x=N, y=Region)) + 
  geom_point() + 
  geom_point(data=filter(hsr.plot.N.data, Region=="KIPP Chicago"),
             color="orange",
             size=2)+
  geom_point(data=filter(hsr.plot.N.data, Region=="KIPP Chicago"),
             shape=1, size=3)+
  theme_bw() +
  theme(axis.text.y = element_text(size=8)) + 
  xlim(0,22) + 
  xlab("Number of Top Performances\n(22 total possible)") + 
  ggtitle("HSR Regional Top Performers\nTotal Top Performer Mentions")



# List of all HSR Questions ####

hsr.avg<-hsr.quests[Measure.Names=="Average"]

hsr.avg.chi <- hsr.avg[Level=="KIPP Chicago"]
hsr.avg.nat <- hsr.avg[Level=="National"]

setkey(hsr.avg.chi, Survey.Question, Parent.Topic, Topic.Name, Role, Measure.Names)
setkey(hsr.avg.nat, Survey.Question, Parent.Topic, Topic.Name, Role, Measure.Names)

hsr.avg.chinat<-hsr.avg.nat[hsr.avg.chi]

setnames(hsr.avg.chinat, 
         c("Measure.Values", "Measure.Values.1"),
         c("Network", "KIPP.Chicago")
)

hsr.avg.chinat[,c("Level", "Level.1"):=NULL]

hsr.avg.chinat[, Magnitude:=KIPP.Chicago/Network]
hsr.avg.chinat[, Difference:=KIPP.Chicago-Network]

hsr.avg.chinat[, Role2:=as.character(Role)]
hsr.avg.chinat[Role2=="School Leader", Role2:="School\nLeader"]

hsr.avg.chinat[, Role2:=factor(Role2, levels=c("Student",
                                              "Parent",
                                              "Teacher",
                                              "School\nLeader",
                                              "Staff"
)
)
]

hsr.avg.chinat[Parent.Topic=="Operations", Parent.Topic:="Ops"]
hsr.avg.chinat[Parent.Topic=="School Support", Parent.Topic:="School\nSupport"]

p.avg2<-ggplot(hsr.avg.chinat, 
               aes(y=Survey.Question, x=factor(0)
               )
) + 
  geom_point(color="black", shape=18, size=3) +
  facet_grid(Parent.Topic~Role2,
             scale="free", 
             space = "free",
             drop = T
  ) + 
  theme_bw() + 
  guides(color="none") +
  theme(axis.text.y=element_text(size=6),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        strip.text=element_text(size=5),
        plot.title=element_text(size=11)
  ) + 
  ylab("") + 
  xlab("")

pdf("graphs/BOD_hsr_questions_2pager.pdf", height=10.5, width=8)
p.avg2 %+% hsr.avg.chinat[Parent.Topic %in% c("Attainment", "Character", "Culture and Climate")] +
  ggtitle("All 203 HSR Survey Questions\nby  Topic and Stakeholder\n(diamonds indicate question asked of role)")
p.avg2 %+% hsr.avg.chinat[!(Parent.Topic %in% c("Attainment", "Character", "Culture and Climate"))]
dev.off()

# 2013-14 MAP performance by region ####

# read data
map.network<-read.csv('data/Historical_MAP_2014 07 21.csv')
map.network<-mutate(map.network, 
                    met_typical=as.integer(gsub("%", "", met_typical)),
                    met_tiered=as.integer(gsub("%", "", met_tiered))
)

# Schools averaged up 
map.school.avgs <- map.network  %>%
  group_by(School_Display_Name, 
           Region_Name, 
           Start_Season, 
           End_Season, 
           Growth_Academic_Year,
           Sub_Test_Name) %>%
  dplyr::summarize(WeightSum=sum(n_count*met_typical), 
                   N=sum(n_count), 
                   Pct_ME=sum(n_count*met_typical)/sum(n_count), 
                   Mean=mean(met_typical),
                   level="School") %>%
  ungroup %>%
  mutate(Region_Name=as.character(Region_Name),
         Start_Season=as.character(Start_Season),
         End_Season=as.character(End_Season),
         Sub_Test_Name=as.character(Sub_Test_Name)
  )

# Anonymize regionsa and single sights

anon_regions<-map.network %>% 
  select(Region_Name) %>% 
  filter(Region_Name!="No Region") %>% 
  unique %>% 
  arrange(as.character(Region_Name)) %>% 
  mutate(Region_Anon=paste("Region", LETTERS[1:n()]))

anon_single_sites<-map.network %>%
  filter(Region_Name=="No Region") %>%
  mutate(Region_Name=School_Display_Name) %>%
  select(Region_Name) %>%
  unique %>%
  arrange(as.character(Region_Name)) %>%
  mutate(Region_Anon=paste("Single Site", c(1:n())))

anon_names <- rbind(anon_regions, anon_single_sites)


# Regions averaged up
map.region.avgs <- map.network  %>%
  group_by(Region_Name, 
           Start_Season, 
           End_Season, 
           Growth_Academic_Year,
           Sub_Test_Name) %>%
  dplyr::summarize(WeightSum=sum(n_count*met_typical), 
                   N=sum(n_count), 
                   Pct_ME=sum(n_count*met_typical)/sum(n_count), 
                   Mean=mean(met_typical),
                   level="Region") %>%
  ungroup %>%
  mutate(School_Display_Name="Region",
         Region_Name=as.character(Region_Name),
         Start_Season=as.character(Start_Season),
         End_Season=as.character(End_Season),
         Sub_Test_Name=as.character(Sub_Test_Name)
  ) 

map.avgs<-rbind(map.region.avgs, map.school.avgs) %>%
  mutate(Region=ifelse(Region_Name=="No Region", School_Display_Name, Region_Name),
         level2=level,
         level2=ifelse(Region_Name=="No Region", "Region", level2),
         Grade="All") %>% 
  left_join(anon_names %>% 
              mutate(Region=Region_Name) %>%
              select(-Region_Name), by = "Region") %>%
  filter(Region!="Region")


#map.avgs[KIPP.Region=="No Region", Region:=as.character(School)]
#map.avgs[KIPP.Region!="No Region", Region:=as.character(KIPP.Region)]

#map.avgs[, level2:=level]
#map.avgs[KIPP.Region=="No Region", level2:="Region"]

#map.avgs[,Grade:="All"]



# Fall to Spring  ####
# Reading
p.fs.reading <- map.avgs %>% filter(Start_Season=="FALL", 
                    End_Season=="SPRING", 
                    Growth_Academic_Year==2014) %>%
  plotMAPRegs(grade = "All", subject="Reading", guides = TRUE, vary.size = TRUE)

# Math 
p.fs.math <- map.avgs %>% filter(Start_Season=="FALL", 
                                    End_Season=="SPRING", 
                                    Growth_Academic_Year==2014) %>%
  plotMAPRegs(grade = "All", subject="Mathematics", guides = TRUE, vary.size = TRUE)

# Spring-to-Spring (1-4, 6-8) ####
# Reading
p.ss.reading<-map.avgs %>% filter(Start_Season=="SPRING", 
                    End_Season=="SPRING", 
                    Growth_Academic_Year==2014) %>%
  plotMAPRegs(grade = "All", subject="Reading", guides = TRUE, vary.size = TRUE)

# Math
p.ss.math<-map.avgs %>% filter(Start_Season=="SPRING", 
                    End_Season=="SPRING", 
                    Growth_Academic_Year==2014) %>%
  plotMAPRegs(grade = "All", subject="Mathematics", guides = TRUE, vary.size = TRUE)

# K-5 F2S / 6-8 S2S ####

# Munge
map.fs.k5 <- map.network %>% filter(Start_Season == "FALL", 
                                    End_Season == "SPRING",
                                    Growth_Grade_Level<=5)

map.ss.678 <- map.network %>% filter(Start_Season == "SPRING", 
                                     End_Season == "SPRING",
                                     Growth_Grade_Level>5)

map.network.fk5s68 <- rbind(map.fs.k5, map.ss.678)

map.school.avgs.fk5s68 <- map.network.fk5s68  %>%
  group_by(School_Display_Name, 
           Region_Name, 
           Growth_Academic_Year,
           Sub_Test_Name) %>%
  dplyr::summarize(WeightSum=sum(n_count*met_typical), 
                   N=sum(n_count), 
                   Pct_ME=sum(n_count*met_typical)/sum(n_count), 
                   Mean=mean(met_typical),
                   level="School") %>%
  ungroup %>%
  mutate(Region_Name=as.character(Region_Name),
         Sub_Test_Name=as.character(Sub_Test_Name)
  )



# Regions averaged up
map.region.avgs.fk5s68 <- map.network.fk5s68  %>%
  group_by(Region_Name, 
           Growth_Academic_Year,
           Sub_Test_Name) %>%
  dplyr::summarize(WeightSum=sum(n_count*met_typical), 
                   N=sum(n_count), 
                   Pct_ME=sum(n_count*met_typical)/sum(n_count), 
                   Mean=mean(met_typical),
                   level="Region") %>%
  ungroup %>%
  mutate(School_Display_Name="Region",
         Region_Name=as.character(Region_Name),
         Sub_Test_Name=as.character(Sub_Test_Name)
  )

map.avgs.fk5s68<-rbind(map.region.avgs.fk5s68, map.school.avgs.fk5s68) %>%
  mutate(Region=ifelse(Region_Name=="No Region", 
                       School_Display_Name, 
                       Region_Name),
         level2=level,
         level2=ifelse(Region_Name=="No Region", "Region", level2),
         Grade="All")


# Reading 
p.fk5s68.reading<-map.avgs.fk5s68 %>% filter(Growth_Academic_Year==2014) %>%
  plotMAPRegs(grade = "All", 
              subject="Reading", 
              guides = TRUE, 
              vary.size = TRUE)
# Math
p.fk5s68.math<-map.avgs.fk5s68 %>% filter(Growth_Academic_Year==2014) %>%
  plotMAPRegs(grade = "All", 
              subject="Mathematics", 
              guides = TRUE, 
              vary.size = TRUE)


# K-2 & 5 F2S / 3-4 & 6-8 S2S (KIPP RC) ####

# Munge

map.fs.foundation <- map.network %>% filter(Start_Season == "FALL", 
                                            End_Season == "SPRING",
                                            Growth_Grade_Level %in% c(0,1,2,5)
)

map.ss.foundation <- map.network %>% filter(Start_Season == "SPRING", 
                                            End_Season == "SPRING",
                                            Growth_Grade_Level %in% c(3,4,6,7,8)
)

map.network.foundation <- rbind(map.fs.foundation, map.ss.foundation) 

map.school.avgs.foundation <- map.network.foundation  %>%
  group_by(School_Display_Name, 
           Region_Name, 
           Growth_Academic_Year,
           Sub_Test_Name) %>%
  dplyr::summarize(WeightSum=sum(n_count*met_typical), 
                   N=sum(n_count), 
                   Pct_ME=sum(n_count*met_typical)/sum(n_count), 
                   Mean=mean(met_typical),
                   level="School") %>%
  ungroup %>%
  mutate(Region_Name=as.character(Region_Name),
         Sub_Test_Name=as.character(Sub_Test_Name)
  )



# Regions averaged up
map.region.avgs.foundation <- map.network.foundation  %>%
  group_by(Region_Name, 
           Growth_Academic_Year,
           Sub_Test_Name) %>%
  dplyr::summarize(WeightSum=sum(n_count*met_typical), 
                   N=sum(n_count), 
                   Pct_ME=sum(n_count*met_typical)/sum(n_count), 
                   Mean=mean(met_typical),
                   level="Region") %>%
  ungroup %>%
  mutate(School_Display_Name="Region",
         Region_Name=as.character(Region_Name),
         Sub_Test_Name=as.character(Sub_Test_Name)
  )

map.avgs.foundation<-rbind(map.region.avgs.foundation, map.school.avgs.foundation) %>%
  mutate(Region=ifelse(Region_Name=="No Region", 
                       School_Display_Name, 
                       Region_Name),
         level2=level,
         level2=ifelse(Region_Name=="No Region", "Region", level2),
         Grade="All")  %>% 
  left_join(anon_names %>% 
              mutate(Region=Region_Name) %>%
              select(-Region_Name), by = "Region") %>%
  filter(Region!="Region") %>%
  
  
map.avgs.foundation <- mutate(map.avgs.foundation,
                              Sub_Test_Name=ifelse(Sub_Test_Name=="ScienceGeneral", "General Science", Sub_Test_Name))

# Reading
p.found.reading <- map.avgs.foundation %>% filter(Growth_Academic_Year==2014) %>%
  plotMAPRegs(grade = "All", 
              subject="Reading", 
              guides = TRUE, 
              vary.size = FALSE,
              anonymize=TRUE,
              show.chicago=TRUE)


# Math
p.found.math <- map.avgs.foundation %>% filter(Growth_Academic_Year==2014) %>%
  plotMAPRegs(grade = "All", 
              subject="Mathematics", 
              guides = TRUE, 
              vary.size = FALSE,
              anonymize=TRUE,
              show.chicago=TRUE)

# Science
p.found.sci <- map.avgs.foundation %>% filter(Growth_Academic_Year==2014) %>%
  plotMAPRegs(grade = "All", 
              subject="General Science", 
              guides = TRUE, 
              vary.size = FALSE,
              anonymize=TRUE,
              show.chicago=TRUE)


# K, 2 & 5 F2S / 1,3,4 & 6-8 S2S (Hybrid) ####

# Munge

map.fs.weird <- map.network %>% filter(Start_Season == "FALL", 
                                       End_Season == "SPRING",
                                       Growth_Grade_Level %in% c(0,2,5)
)

map.ss.weird <- map.network %>% filter(Start_Season == "SPRING", 
                                       End_Season == "SPRING",
                                       Growth_Grade_Level %in% c(1,3,4,6,7,8)
)

map.network.weird <- rbind(map.fs.weird, map.ss.weird)

map.school.avgs.weird <- map.network.weird  %>%
  group_by(School_Display_Name, 
           Region_Name, 
           Growth_Academic_Year,
           Sub_Test_Name) %>%
  dplyr::summarize(WeightSum=sum(n_count*met_typical), 
                   N=sum(n_count), 
                   Pct_ME=sum(n_count*met_typical)/sum(n_count), 
                   Mean=mean(met_typical),
                   level="School") %>%
  ungroup %>%
  mutate(Region_Name=as.character(Region_Name),
         Sub_Test_Name=as.character(Sub_Test_Name)
  )



# Regions averaged up
map.region.avgs.weird <- map.network.weird  %>%
  group_by(Region_Name, 
           Growth_Academic_Year,
           Sub_Test_Name) %>%
  dplyr::summarize(WeightSum=sum(n_count*met_typical), 
                   N=sum(n_count), 
                   Pct_ME=sum(n_count*met_typical)/sum(n_count), 
                   Mean=mean(met_typical),
                   level="Region") %>%
  ungroup %>%
  mutate(School_Display_Name="Region",
         Region_Name=as.character(Region_Name),
         Sub_Test_Name=as.character(Sub_Test_Name)
  )

map.avgs.weird<-rbind(map.region.avgs.weird, map.school.avgs.weird) %>%
  mutate(Region=ifelse(Region_Name=="No Region", 
                       School_Display_Name, 
                       Region_Name),
         level2=level,
         level2=ifelse(Region_Name=="No Region", "Region", level2),
         Grade="All")

# Reading

p.weird.reading<-map.avgs.weird %>% filter(Growth_Academic_Year==2014) %>%
  plotMAPRegs(grade = "All", 
              subject="Reading", 
              guides = TRUE, 
              vary.size = TRUE)
# Math

p.weird.math<-map.avgs.weird %>% filter(Growth_Academic_Year==2014) %>%
  plotMAPRegs(grade = "All", 
              subject="Mathematics", 
              guides = TRUE, 
              vary.size = TRUE)

#map.avgs[KIPP.Region=="No Region", Region:=as.character(School)]
#map.avgs[KIPP.Region!="No Region", Region:=as.character(KIPP.Region)]

#map.avgs[, level2:=level]
#map.avgs[KIPP.Region=="No Region", level2:="Region"]

#map.avgs[,Grade:="All"]


p.read<-p.found.reading
p.math<-p.found.math

p.read
p.math

# Anonymize

regions_anon<-data.frame(Region=map.network[KIPP.Region!="No Region",unique(KIPP.Region)])
regions_anon$N<-c(1:nrow(regions_anon))
regions_anon$Region_Anon<-paste("KIPP Region", LETTERS[regions_anon$N])
#regions_anon$Region_Anon[c(27:29)]<-paste("KIPP Region ", LETTERS[regions_anon$N[c(27:29)]%%26], LETTERS[regions_anon$N[c(27:29)]%%26], sep="")

single.sites<-map.network[KIPP.Region=="No Region", list(Region=unique(School))]
single.sites$N<-c(1:nrow(single.sites))
single.sites$Region_Anon<-paste0("KIPP Single Site ", single.sites$N)

region.ss.anon<-rbind(regions_anon, single.sites) %>% select(Region, Region_Anon)
region.ss.anon<-rbind(region.ss.anon, data.frame(Region="Region", Region_Anon="Region"))
map.avgs.anon<-data.table(left_join(map.avgs, region.ss.anon, by="Region"))

assert_that(nrow(map.avg.anon)==nrow(map.avgs))

map.avg.anon[Region!="Region"]
p.read.anon<-plotMAPRegs(map.avg.anon[Region!="Region"], grade = "All", subject="Reading", guides=TRUE, vary.size = TRUE, anonymize = T, show.above = 7,show.chicago = T)
p.math.anon<-plotMAPRegs(map.avg.anon[Region!="Region"], grade = "All", subject="Mathematics", guides=TRUE, vary.size = TRUE, anonymize = T, show.above = 7,show.chicago = T)

p.read.anon
p.math.anon

pdf("graphs/BOD_MAP_region_anon_overall.pdf", height=10.5, width=8)
grid.arrange(
  p.read.anon,
  p.math.anon,
  ncol=1,
  main="\nFall 2012 - Spring 2013 MAP Percent Meets/Exceeds Typical Growth\nAll Grades Combined")
dev.off()

# pdfs ###

pdf(file=paste0("graphs/MAP_KIPP_Network_", todays_date, ".pdf"), 
    height=10.75,
    width=8.25, onefile=TRUE)


grid.arrange(p.found.reading,
             p.found.math,
             ncol=1,
             main="\n2013 - 2014 MAP Percent Meets/Exceeds Typical Growth\nAll Grades Combined (FS K-2,5 / SS 3-4,6-8)"
             )

grid.arrange(p.fs.reading,
             p.fs.math,
             ncol=1,
             main="\n2013 -  2014 MAP Percent Meets/Exceeds Typical Growth\nAll Grades Combined (FS K-8)"
)


grid.arrange(p.fk5s68.reading,
             p.fk5s68.math,
             ncol=1,
             main="\n2013 -  2014 MAP Percent Meets/Exceeds Typical Growth\nAll Grades Combined (FS K-5 / SS 6-8)"
)

grid.arrange(p.weird.math,
             p.weird.math,
             ncol=1,
             main="\n2013 -  2014 MAP Percent Meets/Exceeds Typical Growth\nAll Grades Combined (FS K,2,5 / SS 1,3 & 6-8)"
)

dev.off()

# 2013-14 MAP Performance ####
map.all.growth.sum.p[Grade<5 & School=="KAMS", School:="KAP"]


p.long<-ggplot(map.all.growth.sum.p[GrowthSeason=="Fall - Spring" & 
                                 Subject %in% c("Reading", "Mathematics", "General Science") & 
                                 School!="Region" &
                                 Grade>1 &
                                 N.S1.S2>=10], 
          aes(x=gsub("20","",SY), 
              y=Pct.Typical*100
              )
          ) + 
  geom_line(aes(group=School, color=School)) +
  geom_point(color="white", size=8.75) +
  geom_hline(aes(yintercept=80), color="lightgray") +
  geom_text(aes(label=paste(Pct.Typical*100,"%",sep=""), 
                       color=School),
            size=2.75) +
  scale_color_manual(values = c("#439539", "purple", "#60A2D7", "#C49A6C")) +
  facet_grid(Subject~Grade) +
  theme_bw() + 
  theme(legend.position="bottom",
        axis.text.x=element_text(size=6.5)) +
  xlab("School Year") +
  ylab("% Meets/Exceeds\nTypical Growth" )

cairo_pdf("graphs/BOD_MAP_long.pdf", height = 8, width=10.5)
p.long + ggtitle("Historical MAP Performance by Grade & Subject\nSY 2010-11 to SY 2013-14")
dev.off()


# Spring to Spring ####
# 2013-14 MAP Performance ####
map.all.growth.sum.p[,School:=factor(School, levels=c("KAP",
                                                      "KAMS",
                                                      "KCCP",
                                                      "KBCP",
                                                      "Region"))]


p.long.s2s<-ggplot(map.all.growth.sum.p[GrowthSeason=="Spring - Spring" & 
                                      Subject %in% c("Reading", "Mathematics", "General Science") & 
                                      School!="Region" &
                                      Grade!=2 &
                                      N.S1.S2>=10], 
               aes(x=gsub("20","",SY), 
                   y=Pct.Typical*100
               )
) + 
  geom_line(aes(group=School, color=School)) +
  geom_point(color="white", size=8.75) +
  geom_hline(aes(yintercept=80), color="lightgray") +
  geom_text(aes(label=paste(Pct.Typical*100,"%",sep=""), 
                color=School),
            size=3) +
  scale_color_manual(values = c("purple", "#439539", "#60A2D7", "#C49A6C")) +
  facet_grid(Subject~Grade) +
  theme_bw() + 
  theme(legend.position="bottom") +
  xlab("School Year") +
  ylab("% Meets/Exceeds\nTypical Growth" )


p.long.f2s<-ggplot(map.all.growth.sum.p[GrowthSeason=="Fall - Spring" & 
                                          Subject %in% c("Reading", "Mathematics", "General Science") & 
                                          School!="Region" &
                                          #Grade>2 &
                                          N.S1.S2>=10], 
                   aes(x=gsub("20","",SY), 
                       y=Pct.Typical*100
                   )
) + 
  geom_line(aes(group=School, color=School)) +
  geom_point(color="white", size=8.75) +
  geom_hline(aes(yintercept=80), color="lightgray") +
  geom_text(aes(label=paste(Pct.Typical*100,"%",sep=""), 
                color=School),
            size=3) +
  scale_color_manual(values = c("purple", "#439539", "#60A2D7", "#C49A6C")) +
  facet_grid(Subject~Grade) +
  theme_bw() + 
  theme(legend.position="bottom") +
  xlab("School Year") +
  ylab("% Meets/Exceeds\nTypical Growth" )

# S2S cohort growth ####

map.growth.2<-map.all.growth.sum.p %>% 
  mutate(Year2=as.integer(gsub("([0-9]+)\\-([0-9]+)", "\\2", SY)), 
         CohortYear=12 - Grade + Year2) 
 
max.grades<-map.growth.2%>%group_by(CohortYear) %>% summarize(CurrentGrade=max(Grade))

map.growth.3<-map.growth.2 %>% left_join(max.grades, 
            by="CohortYear") %>%
  mutate(Cohort=paste0("Class of ", CohortYear, "\nCurrent Grade: ", CurrentGrade),
         School=factor(School, levels=c("KAP", "KAMS", "KCCP", "KBCP")))



p.long.s2s.cohort<-ggplot(filter(map.growth.3,
                                 GrowthSeason=="Spring - Spring",
                                 Subject %in% c("Reading", "Mathematics", "General Science"),
                                 School!="Region",
                                # Grade>2,
                                 N.S1.S2>=10,
                                 CohortYear>=2018,
                                CohortYear!=2024), 
                   aes(x=gsub("20","",SY), 
                       y=Pct.Typical*100
                   )
) + 
  geom_line(aes(group=School, color=School)) +
  geom_point(color="white", size=8.75) +
  geom_hline(aes(yintercept=80), color="lightgray") +
  geom_text(aes(label=paste(Pct.Typical*100,"%",sep=""), 
                color=School),
            size=3) +
  scale_color_manual(values = c("purple", "#439539", "#60A2D7", "#C49A6C")) +
  facet_grid(Subject~Cohort) +
  theme_bw() + 
  theme(legend.position="bottom") +
  xlab("School Year") +
  ylab("% Meets/Exceeds\nTypical Growth" )

# f2s
p.long.f2s.cohort<-ggplot(filter(map.growth.3,
                                 GrowthSeason=="Fall - Spring",
                                 Subject %in% c("Reading", "Mathematics", "General Science"),
                                 School!="Region",
                                 # Grade>2,
                                 N.S1.S2>=10,
                                 CohortYear>=2018), 
                          aes(x=gsub("20","",SY), 
                              y=Pct.Typical*100
                          )
) + 
  geom_line(aes(group=School, color=School)) +
  geom_point(color="white", size=8.75) +
  geom_hline(aes(yintercept=80), color="lightgray") +
  geom_text(aes(label=paste(Pct.Typical*100,"%",sep=""), 
                color=School),
            size=3) +
  scale_color_manual(values = c("purple", "#439539", "#60A2D7", "#C49A6C")) +
  facet_grid(Subject~Cohort) +
  theme_bw() + 
  theme(legend.position="bottom") +
  xlab("School Year") +
  ylab("% Meets/Exceeds\nTypical Growth" )




cairo_pdf("graphs/SL_Retreat_MAP_long.pdf", height = 8, width=10.5, onefile=TRUE)
p.long + ggtitle("Historical MAP Performance by Grade & Subject\nSY 2010-11 to SY 2013-14 (Fall to Spring)")
p.long.s2s + ggtitle("Historical MAP Performance by Grade & Subject\nSY 2010-11 to SY 2013-14 (Spring to Spring)")
dev.off()

p.long.cr<-ggplot(map.all.growth.sum.p[GrowthSeason=="Fall - Spring" & 
                                          Subject %in% c("Reading", "Mathematics", "General Science") & 
                                          School!="Region" &
                                          Grade>1 &
                                          N.S1.S2>=10], 
                   aes(x=gsub("20","",SY), 
                       y=Pct.CR*100
                   )
) + 
  geom_line(aes(group=School, color=School)) +
  geom_point(color="white", size=8.75) +
  geom_hline(aes(yintercept=80), color="lightgray") +
  geom_text(aes(label=paste(Pct.CR*100,"%",sep=""), 
                color=School),
            size=3) +
  scale_color_manual(values = c("#439539", "purple", "#60A2D7", "#C49A6C")) +
  facet_grid(Subject~Grade) +
  theme_bw() + 
  theme(legend.position="bottom") +
  xlab("School Year") +
  ylab("% Meets/Exceeds\nTypical Growth" )


# all together #####
cairo_pdf("graphs/BOD_MAP_all.pdf", height = 8, width=10.5, onefile = TRUE)

  dot.r

  p.avg2 %+% hsr.avg.chinat[Parent.Topic %in% c("Attainment", "Character", "Culture and Climate")] +
    ggtitle("All 203 HSR Survey Questions\nby Parent Topic and Role\n(dots indicate question asked of role)")
  p.avg2 %+% hsr.avg.chinat[!(Parent.Topic %in% c("Attainment", "Character", "Culture and Climate"))]

  grid.arrange(
    p.read.anon,
    p.math.anon,
    ncol=1,
    main="\nFall 2012 - Spring 2013 MAP Percent Meets/Exceeds Typical Growth\nAll Grades Combined")

  p.long + ggtitle("Historical MAP Performance by Grade & Subject\nSY 2010-11 to SY 2013-14")

dev.off()


# HSR Survey Summary ####
# data from one pagers on hsr.kipp.org

hrs.recs<-hsr.all %>% filter(grepl("recommend", Survey.Question),
                   grepl("Positive", Measure.Names),
                   Role!="Staff"
                   ) %>%
  mutate(Prompt=paste0(Role, 
                       "s", 
                       str_replace(Survey.Question, "I ", " ")
                      ),
         Prompt=str_replace(Prompt, "my", "their"),
         School=abbrev(School, exceptions = list(old=c("KAPS", "KCMS"), 
                                                          new=c("KAP", "KCCP")
                                                          )
                                ),
         School=factor(School, levels=c("KAP", "KAMS", "KCCP", "KBCP")),
         Percent=as.numeric(as.character(Measure.Values))*100
         )

p.hsr.recs<-ggplot(hrs.recs, aes(x=Percent, y=Prompt)) + 
  geom_point(aes(color=School),
             size=3, 
             shape=17,
             alpha=0.6) + 
  theme_bw() + 
  theme(legend.position="bottom") +
  scale_color_manual(values = c("purple",
                                "#439539", 
                                "#60A2D7", 
                                "#C49A6C")
                     )

# HSR High Low Quesitons ####
hsr.hilo <- hsr.all %>% 
  filter(School=="KIPP.Chicago",
         Measure.Names=="Average"
         ) %>%
  arrange(Role, Measure.Values) %>%
  group_by(Role) %>%
  mutate(Avg_Rank=row_number(Measure.Values)) %>%
  filter(Avg_Rank==max(Avg_Rank) | Avg_Rank==min(Avg_Rank)) %>%
  mutate(Rank=ifelse(Avg_Rank!=1, "High", "Low"),
         Rank2=factor(as.character(Rank),levels=c("Low","High"), ordered=T)
  )


p.sample_questions<-ggplot(hsr.hilo, aes(x=Measure.Values, y=Survey.Question)) + 
  geom_point() + 
  facet_grid(Role~., drop = T, scales="free_y") + 
  theme_bw() 
  
p.sample_questions_colors<-ggplot(hsr.hilo, aes(x=0, y=factor(Rank2))) + 
  geom_text(aes(label=Survey.Question, color=Rank), hjust=0) + 
  facet_grid(Role~., drop = T, scales="free_y") + 
  scale_color_manual(values = c("#439539",
                                "#C49A6C")) +
  theme_bw() +
  theme(legend.position="bottom",
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.grid=element_blank(),
        strip.text.y=element_text(size=9),
        legend.margin=unit(.005, "mm"),
        axis.ticks.margin=unit(.005, "mm")) +
  xlim(0,10) 



# Back Page Summary ####

summ.12<-data.frame(
  Metric = c("Enrollment", "FRM Students", "Attendance", "Special Needs", "Student Attrition", "Teacher Retention"),
  Goal = c("1,038 students" ,"> 90%", "> 95 %", " CPS Avg. ", " < 10%", "> 75%"),
  "SY2014" = c(990, "93%", "95%", "12%", "8% (est.)", "72%"), 
  "SY2013" = c(746, "93%", "94%", "12%", "16%", "91%")
  )

summ.table<-tableGrob(summ.12, 
                      show.rownames=FALSE, 
                      cols=c("Metric", "SY 2014 Goal", "SY2014", "SY2013"))

summ.table

# SQRP Results ###
sqrp<-data.frame(
  Metric = c("SQRP Rating\n(5 tiers with Tier 1 is best)", "SQRP Points\n(out of 5)"),
  KA = c("Tier 1", "4.3"),
  KCCP = c("Tier 1", "4.6"),
  KBCP = c("Tier 1 or Tier 2" , "3.8 - 4.2")
)

sqrp.table<-tableGrob(sqrp, show.rownames=FALSE, cols = c("Metric",
                                                          "KIPP Ascend",
                                                          "KIPP Create",
                                                          "KIPP Bloom"))


# First page ala hsr summaries ####

g.stu<-arrangeGrob(
  textGrob("Students who would recommend\nKIPP to their friends and family."),
  tableGrob(data.frame("KIPPChicago"=3.97, "KIPPNational"=3.52), 
            cols=c("KIPP Chicago", "KIPP Network"),
            show.rownames = FALSE),
  textGrob("70% of students agree that\nthey would recommend KIPP."),
  ncol=1,
  nrow=3
  )

g.stu.2<-gList(rectGrob(x=.5, y=.5, width=unit(1, "native")), g.stu)

g.famlies<-arrangeGrob(
  textGrob("Families who are satisfied or very\nsatisfied with their school."),
  tableGrob(data.frame(Region=4.53, Network=3.39), 
            cols=c("KIPP Chicago", "KIPP Network"),
            show.rownames = FALSE),
  textGrob("92% of families are satisfied\nwith their child's school."),
  ncol=1,
  nrow=3
)
g.families.2<-gList(rectGrob(x=.5, y=.5, width=unit(1, "native")), g.famlies)


g.teachers<-arrangeGrob(
  textGrob("Teachers who would recommend\nKIPP as a great place to work."),
  tableGrob(data.frame(Region=3.95, Network=3.67), 
            cols=c("KIPP Chicago", "KIPP Network"),
            show.rownames = FALSE),
  textGrob("73% of teachers would\n recommend KIPP as a great\nplace to work."),
  ncol=1,
  nrow=3
  )

g.teachers.2<-gList(rectGrob(x=.5, y=.5, width=unit(1, "native")), g.teachers)


g.tables<-arrangeGrob(gTree(children = g.stu.2), 
                      gTree(children = g.families.2), 
                      gTree(children = g.teachers.2),
                      nrow=1, 
                      ncol=3,
                      widths=unit(2.75, "inches")
)
g.tables




# EOY Summarry 11x17 ####


p.avg3<-ggplot(hsr.avg.chinat, 
               aes(y=Survey.Question, x=factor(0)
               )
) + 
  geom_point(aes(color=Magnitude,
                 shape=Magnitude<1),
             size=1.25) +
  geom_text(aes(x=.90,
                label=KIPP.Chicago), 
            size=1.5,
            hjust=1
  ) + 
  scale_color_gradient2(low="red", mid="gray", high="green",
                        midpoint=1
  ) +
  scale_shape_manual(values = c(19,4)) +
  facet_grid(Parent.Topic~Role2,
             scale="free", 
             space = "free",
             drop = T
  ) + 
  theme_bw() + 
  guides(color="none", 
         shape="none") +
  theme(axis.text.y=element_text(size=4),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        strip.text=element_text(size=5)
  ) + 
  ylab("") + 
  xlab("")

# Student Transfers ####
date.first  <- "2013-08-19" # first day of school year

date.second <- floor_date(today(), unit="week") 

date.title <- date.second -weeks(1) + days(1) 

title.date<-paste(lubridate::month(date.title, 
                                   label=TRUE, 
                                   abbr=FALSE),
                  " ",
                  lubridate::day(date.title), ", ", 
                  lubridate::year(date.title), sep=""
)

source('src/transfer_tables.R')


detach("package:plyr")


enrolled<-rbind(group_by(Enrolled.121003, SCHOOLID) %>% 
                  dplyr::summarise(N=n()) %>% 
                  mutate(Year="SY12-13"), 
                group_by(Enrolled.131001, SCHOOLID) %>% 
                  dplyr::summarise(N=n()) %>% 
                  mutate(Year="SY13-14")
)

enrolled$School<-mapply(function(x){ switch(as.character(x),
                                            "7810" = "KAMS",
                                            "78102" = "KAP",
                                            "400146"= "KCCP",
                                            "400163" = "KBCP"
)
},
enrolled$SCHOOLID
)

enrolled<-mutate(enrolled, 
                 School=factor(School, 
                               levels=c("KAP", "KAMS", "KCCP", "KBCP")
                 )
)

xferplot<-left_join(xferplot, 
                    select(enrolled, -SCHOOLID), 
                    by=c("School", "Year")
) 

xferplot <- mutate(xferplot, Pct=round(Value/N*100))

todays_month<-as.character(lubridate::month(today(), label = TRUE, abbr = TRUE))

#remove cumulative transfers passed this month
xferplot2<-xferplot[!(xferplot$Year=="SY13-14" & 
                        xferplot$Month >= todays_month & 
                        xferplot$Variable=="Cumulative Transfers"),]

xferplot2.nm<-xferplot.nm[!(xferplot.nm$Year=="SY13-14" & 
                              xferplot.nm$Month >= todays_month & 
                              xferplot.nm$Variable=="Cumulative Transfers"),]

p.transfers<-ggplot(data=subset(xferplot2, Variable=="Ceiling"), 
       aes(x=Month, y=Value)) + 
  geom_area(data=subset(xferplot2, Variable!="Ceiling"), 
            aes(x=Month, y=CumVal, fill=School, group=School), 
            stat='identity',
            #fill="#439539", 
            width=.5, 
            alpha=.4) + 
  geom_area(data=subset(xferplot2.nm, Variable!="Ceiling"), 
            aes(x=Month, y=CumVal, fill=School, group=School), 
            stat='identity',
            #fill="#439539", 
            width=.5, 
            alpha=1) + 
  geom_line(aes(group=Variable), color="#E27425") + 
  geom_text(data=subset(xferplot2, Variable!="Ceiling"), 
            aes(x=Month, 
                y=Value, 
                group=Variable, 
                label=paste0(Value,"\n(",Pct,"%)")), 
            size=2.5,
            vjust=0) +
  geom_text(data=subset(xferplot2.nm, Variable!="Ceiling"), 
            aes(x=Month, 
                y=Value, 
                group=Variable, 
                label=Value), 
            size=2.5,
            vjust=1) +
  facet_grid(Year~School, scale="free_y") +
  scale_fill_manual(values = c("purple",  #KCCP 
                               "#439539", #KAMS
                               "#60A2D7", #KCCP
                               "#C49A6C"  #KBCP
  )
  ) + theme_bw() + 
  theme(axis.title = element_blank(),
        axis.text.x = element_text(size=7,angle=45, hjust=1),
        axis.text.y = element_text(size=7),
        strip.text = element_text(size=7),
        legend.position="none"
  )     


# Residual Analysis ####
source("../SQRP/src//SQRP_CPS_KIPP_Reconciliation_Post_Release_2.R")

# plot residuals rankked
p.resid <- 
  ggplot(nwea_resid, 
       aes(y=Residual, 
           x=Rank_Resid
           )
       ) +
  geom_segment(aes(color=School, 
                   xend=Rank_Resid,
                   yend=0
                   )
               ) +
  geom_point(data=filter(nwea_resid, KIPP==TRUE),
             aes(color=School)
             ) +
  geom_text(data=filter(nwea_resid, KIPP==TRUE),
            aes(label=School,
                color=School, 
                y=ifelse(School=="Ascend" & Subject=="Reading", 
                         Residual+10,
                         ifelse(School=="Create" & Subject=="Reading",
                                Residual-2,
                                Residual+6
                                )
                         ),
                x=ifelse(School=="Create" & Subject=="Reading", Rank_Resid-15, Rank_Resid)
                ),
            size=3
            ) +
  geom_hline(aes(yintercept=0)) +
  scale_color_manual(values=c("#439539", #Ascend
                            "#60A2D7", #Create
                            "purple", #Bloom
                            "#CFCCC1", #Non-charter
                            "#F7941E" #Charter
                            )
                   ) +
  facet_grid(Subject~.) +
  theme_bw() + 
  theme(legend.position="bottom") +
  xlab("Rank by Residual Value") +
  ylab("Residuals, 2013 - 2014\n(Actual - Predicted Growth Percentile)") 
  




todays_date <-format(today(), "%y%m%d")

# page 1 ####
page1<-arrangeGrob(g.tables,
            p.sample_questions_colors + ggtitle("8 Highest and Lowest Scoring HSR Questions by Stakeholder\n(out of 203 HSR Questions)"),
            hsr.top.2 + ylab(""), 
            ncol=1, 
            nrow=3, 
            heights=c(0.4,.7,.9),
            main=textGrob("\nHealthy Schools and Regions Survey Summary\n",
                          gp=gpar(fontsize=15, fontface="bold"))
)

# page 2 ####
page2<-arrangeGrob( p.found.reading + 
               theme(axis.text.y=element_text(size=7),
                     legend.position="none") +
                 ggtitle("2013 - Spring 2014 MAP Percent Meets/Exceeds Typical Growth\nAll Grades Combined (FS K-2,5 / SS 3-4,6-8)"),
             p.found.math +
               theme(axis.text.y=element_text(size=7),
                     legend.position="none"),
             ncol=1,
             nrow=2,
             heights=c(1,.9),
             main=textGrob("\nKIPP Network SY 2013-14 MAP Performance", 
                           gp=gpar(fontsize=15, fontface="bold"))
)

# page 3 ####
page3<-arrangeGrob( p.found.sci +
               theme(axis.text.y=element_text(size=6.5),
                     legend.position="left"),
             p.resid + ggtitle("KIPP Chi vs. CPS\nResidual Analysis"), 
             # p.long.s2s +ggtitle("Spring 2013 to Spring 2014"), 
             ncol=1,
             nrow=2,
             heights=c(1,2),
             main=textGrob("\nKIPP Network & CPS MAP Performance",  
                           gp=gpar(fontsize=15, fontface="bold"))
)

# page 4 ####
page4<-arrangeGrob(summ.table, 
            arrangeGrob(textGrob("School Quality Rating Policy\nEstimated Ratings and Points.", 
                                 vjust = .7,
                                 gp=gpar(fontsize=15)),
                        sqrp.table,
                        heights=c(.25,.5)
            ),
            p.transfers + ggtitle("HSR Student Transfers\n as of 8/14/2014"),
            main=textGrob("\nSummary Statistics",
                          gp=gpar(fontsize=15, fontface="bold")),
            ncol=1, 
            widths=1, 
            nrow=3, 
            heights=c(1,1,2))

theme_adjustments<-theme(strip.text.y=element_text(size=7),
                         axis.text.x=element_text(size=6)
                         )

# Page MAP longitude ####
page.map.insert.1<-arrangeGrob(p.long.f2s + theme_adjustments +
                                 ggtitle("Fall-to-Spring"),
                               p.long.s2s + theme_adjustments +
                                 guides(color="none", 
                                        alpha="none", 
                                        size="none", 
                                        shape="none") +
                                 ggtitle("Spring-to-Spring") + 
                                 theme(strip.text.y=element_text(size=5.5)),
                               ncol=1,
                               nrow=2,
                               heights=c(3,2),
                               main=textGrob("\nMAP Longitudinal Results by Grade-Level",
                                             gp=gpar(fontsize=15, fontface="bold")
                                             )
                               )

page.map.insert.2<-arrangeGrob(p.long.f2s.cohort + theme_adjustments +
                                 ggtitle("Fall-to-Spring"),
                               p.long.s2s.cohort + theme_adjustments +
                                 guides(color="none", 
                                        alpha="none", 
                                        size="none", 
                                        shape="none") +
                                 ggtitle("Spring-to-Spring")+ 
                                 theme(strip.text.y=element_text(size=5.5)),
                               ncol=1,
                               nrow=2,
                               heights=c(3,2),
                               main=textGrob("\nMAP Longitudinal Results by Cohort",
                                             gp=gpar(fontsize=15, fontface="bold")
                                             )
                               )


# 11 x 17 ####
pdf(paste0("graphs/EOY_BOD_11_x_17_",todays_date, ".pdf"), width=17, height=11, onefile = TRUE)
# pages 1 & 4
grid.arrange(
  page4,
  page1,
  ncol=2,
  widths=c(1,1)
  )

# pages 2-3 
grid.arrange( 
             page2, 
             page3,
             ncol=2, 
             widths=c(1,1)
             )
dev.off()

# 8.5 x 11 ####
pdf(paste0("graphs/EOY_BOD_8.5_x_11_",todays_date, ".pdf"), width=8.5, height=11, onefile = TRUE)
page1
page2
page3
page4

page.map.insert.1
page.map.insert.2

dev.off()

pdf(paste0("graphs/EOY_BOD_MAP_Long",todays_date, ".pdf"), width=10.75, height=8.25, onefile = TRUE)
page.map.insert.1
page.map.insert.2
dev.off()


