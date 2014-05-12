map.network<-copy(RIT.pctTypical.bySchool.ForChris.2014.05.01)

map.network[,Subtest.Name:=gsub("(MAP\\.)(.+)", "\\2", Subtest.Name)]


map.regions<-map.network[, list(.N, level="Region"), 
                          by=list(Subtest.Name,
                               KIPP.Region.ID,
                               KIPP.Region,
                               Grade,
                               Start_RIT=Reg_Avg_Start_RIT,
                                End_RIT=Reg_Avg_End_RIT,        
                                Pct_ME=Reg_pct_met_typical_growth
                                  )
                      ]

map.regions[, N:=NULL]
map.regions[,School:=KIPP.Region]
map.regions<-map.regions[!is.na(Start_RIT)]


map.schools<-map.network[,list(Subtest.Name,
                               School,
                               KIPP.Region.ID, 
                               KIPP.Region, 
                               Grade,
                               Start_RIT=Sch_Avg_Start_RIT,
                               End_RIT=Sch_Avg_End_RIT,
                               Pct_ME=Sch_pct_met_typical_growth,
                               level="School")]


map.combined<-rbind(map.regions, map.schools)

map.combined[, Rank_ME:=rank(Pct_ME),
               by=list(Subtest.Name, Grade)]

map.combined[KIPP.Region=="No Region", Region:=as.character(School)]
map.combined[KIPP.Region!="No Region", Region:=as.character(KIPP.Region)]

map.combined[, level2:=level]
map.combined[KIPP.Region=="No Region", level2:="Region"]


# subset data since rankings will vary accross subjects and grades

plotMAPRegs<-function(.data=map.combined, 
                      grade="5th Grade", 
                      subject="Mathematics",
                      guides=FALSE){
  plot.data<-.data[Grade==grade & Subtest.Name==subject]
  

  regions<-plot.data[level2=="Region", as.character(Region)][order(plot.data[level2=="Region",Pct_ME])]
  
  plot.data[,Region:=factor(as.character(Region),levels=regions)]
  
  
  p<-ggplot(plot.data, 
            aes(x=Pct_ME, y=Region)) + 
    geom_point(aes(shape=level, size=level, alpha=level)) +
    geom_point(data=plot.data[Region=="KIPP Chicago"],
               aes(shape=level, 
                   size=level,
                   alpha=level),
               color="orange")+
    scale_size_manual(values = c(3,2)) +
    scale_alpha_manual(values = c(1,.5)) +
    #geom_text(aes(x=min(Pct_ME)-2, label=KIPP.Region)) +
    facet_grid(Grade~Subtest.Name, scales = "free_y", space="free_y") +
    theme_bw()
  if(!guides) p<- p+ guides(color="none", 
                            alpha="none", 
                            size="none", 
                            shape="none")
  p
  
}
plot.data<-map.combined[Grade=="5th Grade" & Subtest.Name=="Mathematics"]



regions<-plot.data[level2=="Region", as.character(Region)][order(plot.data[level2=="Region",Pct_ME])]

plot.data[,Region:=factor(as.character(Region),levels=regions)]


p<-ggplot(plot.data, 
       aes(x=Pct_ME, y=Region)) + 
  geom_point(aes(shape=level, size=level, alpha=level)) +
  geom_point(data=plot.data[Region=="KIPP Chicago"],
                            aes(shape=level, 
                                size=level,
                                alpha=level),
                            color="orange")+
  scale_size_manual(values = c(3,2)) +
  scale_alpha_manual(values = c(1,.5)) +
  #geom_text(aes(x=min(Pct_ME)-2, label=KIPP.Region)) +
  facet_grid(Grade~Subtest.Name, scales = "free_y", space="free_y") +
  theme_bw() + guides(color="none", alpha="none", size="none", shape="none")
p

pdf("graphs/MAP_ME_test.pdf", height=11, width=8.5)
p
dev.off()

p<-list()
for (s in c("Reading", "Mathematics")){
  for (g in setdiff(unique(map.combined$Grade),c("3rd Grade", "4th Grade"))){
    p[[paste(s, 
            str_extract(g, pattern = "."), 
            sep="_")]]<- plotMAPRegs(grade=g, subject=s)
  }
}

#Middle
pdf("graphs/MAP_region_all_grades.pdf", height=10.75, width=8)
grid.arrange(
  p$Reading_K,
  p$Mathematics_K,
  ncol=1,
  main="\nFall 2012 - Spring 2013 MAP Percent Meets/Exceeds\nKindergarten")

grid.arrange(
  p$Reading_1,
  p$Mathematics_1,
  ncol=1,
  main="\n1st Grade")

grid.arrange(
  p$Reading_2,
  p$Mathematics_2,
  ncol=1,
  main="\n2nd Grade")


grid.arrange(
             p$Reading_5,
             p$Mathematics_5,
             ncol=1,
             main="\n5th Grade")

grid.arrange(
  p$Reading_6,
  p$Mathematics_6,
  ncol=1,
  main="\n6th Grade")

grid.arrange(
  p$Reading_7,
  p$Mathematics_7,
  ncol=1,
  main="\n7th Grade")

grid.arrange(
  p$Reading_8,
  p$Mathematics_8,
  ncol=1,
  main="\n8th Grade")


dev.off()

# Regional Roll-ups (i.e, no separated by grade) ####
map.school.avgs<-map.network[,list(WeightSum=sum(Sch_N_tested*Sch_pct_met_typical_growth), 
                  N_Students=sum(Sch_N_tested), 
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
                                   N_Students=sum(Sch_N_tested), 
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


map.avgs<-rbind(map.region.avgs, map.school.avgs)



map.avgs[KIPP.Region=="No Region", Region:=as.character(School)]
map.avgs[KIPP.Region!="No Region", Region:=as.character(KIPP.Region)]

map.avgs[, level2:=level]
map.avgs[KIPP.Region=="No Region", level2:="Region"]

map.avgs[,Grade:="All"]


p.read<-plotMAPRegs(map.avgs, grade = "All", subject="Reading", guides = TRUE)
p.math<-plotMAPRegs(map.avgs, grade = "All", subject="Mathematics", guides=TRUE)

p.read
p.math

pdf("graphs/MAP_region_overall.pdf", height=11, width=8.5)
grid.arrange(
  p.read,
  p.math,
  ncol=1,
  main="\nFall 2012 - Spring 2013 MAP Percent Meets/Exceeds\nAll Grades Combined")
2dev.off()
