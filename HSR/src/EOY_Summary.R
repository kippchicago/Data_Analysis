# May BOD meeting script

# Need HSR dots with top quartile of quartile's named, others' anonymized



require(ProjectTemplate)
load.project()

# Rgion Top Quartile Performers ####

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

# 2012-13 MAP performance by region ####
map.network<-copy(RIT.pctTypical.bySchool.ForChris.2014.05.01)

map.network[,Subtest.Name:=gsub("(MAP\\.)(.+)", "\\2", Subtest.Name)]


map.school.avgs<-map.network[,list(WeightSum=sum(Sch_N_tested*Sch_pct_met_typical_growth), 
                                   N=sum(Sch_N_tested), 
                                   Pct_ME=sum(Sch_N_tested*Sch_pct_met_typical_growth)/sum(Sch_N_tested), 
                                   Mean=mean(Sch_pct_met_typical_growth),
                                   level="School"
), 
by=list(School, 
        KIPP.Region, 
        Subtest.Name
)
]


map.region.avgs<-map.network[,list(WeightSum=sum(Sch_N_tested*Sch_pct_met_typical_growth), 
                                   N=sum(Sch_N_tested), 
                                   Pct_ME=sum(Sch_N_tested*Sch_pct_met_typical_growth)/sum(Sch_N_tested), 
                                   Mean=mean(Sch_pct_met_typical_growth),
                                   level="Region"
), 
by=list( 
  KIPP.Region, 
  Subtest.Name
)
]

map.region.avgs[,School:="Region"]


map.avgs<-rbind(map.region.avgs, map.school.avgs, use.names=TRUE)



map.avgs[KIPP.Region=="No Region", Region:=as.character(School)]
map.avgs[KIPP.Region!="No Region", Region:=as.character(KIPP.Region)]

map.avgs[, level2:=level]
map.avgs[KIPP.Region=="No Region", level2:="Region"]

map.avgs[,Grade:="All"]

plotMAPRegs<-function(.data=map.combined, 
                      grade="5th Grade", 
                      subject="Mathematics",
                      guides=FALSE, 
                      vary.size=FALSE, 
                      anonymize=FALSE,
                      show.above=7,
                      show.chicago=TRUE){
  
  plot.data<-copy(.data[Grade==grade & Subtest.Name==subject])
  
  if(anonymize){
    plot.data[,Region2:=Region]
    plot.data[,Region:=Region_Anon]
    
    
    regions<-plot.data[level2=="Region", as.character(Region)][order(plot.data[level2=="Region",Pct_ME])]
    plot.data[,Region:=factor(as.character(Region),levels=regions)]
    
    max.rank<-max(as.numeric(plot.data$Region))
    plot.data[as.numeric(Region) %in% c((max.rank-show.above):max.rank), Region:=Region2]
    
    if(show.chicago) plot.data[KIPP.Region=="KIPP Chicago", Region:=Region2]
    
    regions<-plot.data[level2=="Region", as.character(Region)][order(plot.data[level2=="Region",Pct_ME])]
    plot.data[,Region:=factor(as.character(Region),levels=regions)]
      
  } else {
    regions<-plot.data[level2=="Region", as.character(Region)][order(plot.data[level2=="Region",Pct_ME])]
    
    plot.data[,Region:=factor(as.character(Region),levels=regions)]  
  }
  
  
  p<-ggplot(plot.data, 
            aes(x=Pct_ME, y=Region))
  
  if(!vary.size) {
    p <- p + geom_point(aes(shape=level, size=level, alpha=level)) +
      geom_point(data=plot.data[Region=="KIPP Chicago"],
                 aes(shape=level, 
                     size=level,
                     alpha=level),
                 color="orange")+
      scale_size_manual(values = c(3,2)) +
      scale_alpha_manual(values = c(1,.5))
  } else {
    p <- p + geom_point(aes(shape=level, size=N, alpha=level)) +
      geom_point(data=plot.data[Region=="KIPP Chicago"],
                 aes(shape=level, 
                     size=N,
                     alpha=level),
                 color="orange") +
      #scale_size_manual(values = c(3,2)) +
      scale_alpha_manual(values = c(1,.5))  +
      scale_size_continuous("Total\nStudents", 
                            range = c(2, 4), 
                            breaks=c(250,500,1000,2000)) 
  }
  p<- p + facet_grid(Grade~Subtest.Name, scales = "free_y", space="free_y") +
    scale_x_continuous(breaks=c(.4, .6, .8), 
                     labels=c("40%", "60%", "80%")
                     ) +
    theme_bw() 
  if(!guides) p<- p+ guides(color="none", 
                            alpha="none", 
                            size="none", 
                            shape="none")
  p + xlab("Percent Meets/Exceeds Typcial Growth")
  
}





p.read<-plotMAPRegs(map.avgs, grade = "All", subject="Reading", guides = TRUE, vary.size = TRUE)
p.math<-plotMAPRegs(map.avgs, grade = "All", subject="Mathematics", guides=TRUE, vary.size = TRUE)

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




# 2013-14 MAP Performance ####

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
            size=3) +
  scale_color_manual(values = c("#439539", "purple", "#60A2D7", "#C49A6C")) +
  facet_grid(Subject~Grade) +
  theme_bw() + 
  theme(legend.position="bottom") +
  xlab("School Year") +
  ylab("% Meets/Exceeds\nTypical Growth" )

cairo_pdf("graphs/BOD_MAP_long.pdf", height = 8, width=10.5)
p.long + ggtitle("Historical MAP Performance by Grade & Subject\nSY 2010-11 to SY 2013-14")
dev.off()


# Spring to Spring ####
# 2013-14 MAP Performance ####

p.long.s2s<-ggplot(map.all.growth.sum.p[GrowthSeason=="Spring - Spring" & 
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
            size=3) +
  scale_color_manual(values = c("#439539", "purple", "#60A2D7", "#C49A6C")) +
  facet_grid(Subject~Grade) +
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


pdf("graphs/11_x_17_test.pdf", width=17, height=11, onefile = TRUE)
# pages 1-3
grid.arrange(
  arrangeGrob(p.avg3, main="\nAll 2003 HSR Survery Questions", ncol=1, widths=1),
  dot.r,
  ncol=2,
  widths=c(1,1)
  )


# pages 2-3
grid.arrange( 
             arrangeGrob( p.read,
                          p.math,
                          ncol=1,
                          main="\nFall 2012 - Spring 2013 MAP Percent Meets/Exceeds Typical Growth\nAll Grades Combined"
                          ), 
             arrangeGrob(p.long +ggtitle("Fall 2013 to Spring 2014"), 
                         p.long.s2s +ggtitle("Spring 2013 to Spring 2014"), 
                         ncol=1,
                         main="\nHistorical MAP Performance"
                         ),
             ncol=2, 
             widths=c(1,1)
             )


dev.off()



