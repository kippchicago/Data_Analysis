# HSR regional performers script

require(ProjectTemplate)
load.project()

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


p<-ggplot(hsr.plot.data,
       aes(x=Role, y=Region)) + 
  geom_tile(data=hsr.plot.data[Role!="Total"], aes(alpha=N)) +
  geom_tile(data=hsr.plot.data[Role=="Total"], aes(alpha=N/3.2)) +
  geom_text(aes(label=N)) +
  theme_bw() + 
  ggtitle("HSR Regional Top Performers\nCount of Top Quartile Parent Topics by Role")

p

cairo_pdf("graphs/hsr_Regional_Quartile_Count.pdf", width=8.5, height=11)
  p
dev.off()

png("graphs/hsr_Regional_Quartile_Count.png", width=8.5, height=11,units = "in", res=100)
p
dev.off()

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
dot<-ggplot(hsr.plot.data,
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
  ggtitle("HSR Regional Top Performers\nCount of Top Quartile Parent Topics by Role")
dot

pdf("graphs/hsr_dots_alpha.pdf", width=8.5, height=11)
  dot
dev.off()

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
  ggtitle("HSR Regional Top Performers\nCount of Top Quartile Parent Topics by Role")
dot.r

pdf("graphs/hsr_dots_ranked.pdf", width=8.5, height=11)
 dot.r
dev.off()

# Non-anonymous ####
hsr.plot.data[,Region_N:=paste(Region, "(", N, ")")]


region_N_ranked<-hsr.plot.data[,list(Region_N, N)][order(N)][,unique(Region_N)]
#reorder regions using total top quartile parent topics

region_topic_N<-data.table(expand.grid(Region_N=unique(hsr.plot.data$Region_N), RPT=ptopics$RPT))


#extrat roles and parent topics from rpt
region_topic_N[,Role:=gsub("(.+)\\.(.+)", "\\1", RPT)]
region_topic_N[,Parent_Topic:=gsub("(.+)\\.(.+)", "\\2", RPT)]


region_topic_N[,Region_N:=factor(Region_N, 
                                          levels=region_N_ranked)]


hsr.plot.data[,Region_Anon_N:=factor(Region_Anon_N, 
                                     levels=region_anon_ranked)]

dot.r.named<-ggplot(hsr.plot.data,
              aes(x=Parent_Topic, y=Region_N)) + 
  geom_dotplot(data=region_topic_N, 
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
  ggtitle("HSR Regional Top Performers\nCount of Top Quartile Parent Topics by Role")
dot.r.named

pdf("graphs/hsr_dots__named_ranked.pdf", width=8.5, height=11)
dot.r.named
dev.off()