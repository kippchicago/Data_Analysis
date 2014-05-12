# HSR schoolal performers script

require(ProjectTemplate)
load.project()

# aggreate schinal data to unique instances of schoolal, role, parent.topic
hsr.sch<-copy(hsr.schools[,list(Count=.N), by=list(School,
                                        Rank,
                                        Role, 
                                        Parent_Topic)][,Count:=NULL])
hsr.sch.role.tot<-hsr.sch[, .N, by=list(School, Role)]

hsr.sch.sum<-hsr.sch.role.tot[,list(Role="Total", N=sum(N)), by=School]

hsr.plot.data<-rbind(hsr.sch.role.tot, hsr.sch.sum)

hsr.plot.data[,Role:=factor(Role, 
                            levels=c("Students", 
                                     "Parent", 
                                     "Teachers", 
                                     "Staff",
                                     #"School Leader",
                                     "Total"))]

hsr.plot.data[,School:=factor(School, 
                              levels=hsr.sch.sum$School[order(hsr.sch.sum$N)])]


p<-ggplot(hsr.plot.data,
       aes(x=Role, y=School)) + 
  geom_tile(data=hsr.plot.data[Role!="Total"], aes(alpha=N)) +
  geom_tile(data=hsr.plot.data[Role=="Total"], aes(alpha=N/3.2)) +
  geom_text(aes(label=N)) +
  theme_bw() + 
  ggtitle("HSR Schoolal Top Performers\nCount of Top Quartile Parent Topics by Role")

p

cairo_pdf("graphs/hsr_Schoolal_Quartile_Count.pdf", width=8.5, height=11)
  p
dev.off()

png("graphs/hsr_Schoolal_Quartile_Count.png", width=8.5, height=11,units = "in", res=100)
p
dev.off()

# dot plot ####
# need to do some munging to get the cartesian product of each Role's parent topics x
# each school.

# first, let's get a vector of all schools in the data
schools<-hsr.sch[,unique(School)]

#now we need to get unique roles
roles<-hsr.sch[,unique(Role)]
roles

#and for each role we need unique parent topics
pt.list<-lapply(roles, 
               function(x) hsr.sch[Role==x, 
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

# return cartesion product of school by RPT
school_topic<-data.table(expand.grid(School=schools, RPT=ptopics$RPT))

#extrat roles and parent topics from rpt
school_topic[,Role:=gsub("(.+)\\.(.+)", "\\1", RPT)]
school_topic[,Parent_Topic:=gsub("(.+)\\.(.+)", "\\2", RPT)]


#reorder schools using total top quartile parent topics
school_topic[,School:=factor(School, 
                             levels=hsr.sch.sum$School[order(hsr.sch.sum$N)])]
hsr.sch[,School:=factor(School, 
                              levels=hsr.sch.sum$School[order(hsr.sch.sum$N)])]



hsr.plot.data<-data.table(left_join(hsr.sch, hsr.sch.sum, by=c("School")))

hsr.plot.data[,Role.y:=NULL]
setnames(hsr.plot.data, "Role.x", "Role")

for (r in 1:length(schools)){
  hsr.plot.data[School==schools[r] & N>=10,School_Anon:=School]
  hsr.plot.data[School==schools[r] & N<10,School_Anon:=paste("KIPP School", LETTERS[r])]
}

hsr.plot.data[,School_Anon_N:=paste(School_Anon, "(", N, ")")]

schools_anons<-hsr.plot.data[,unique(School_Anon_N)]

school_topic_anons<-data.table(expand.grid(School_Anon_N=schools_anons, RPT=ptopics$RPT))

#extrat roles and parent topics from rpt
school_topic_anons[,Role:=gsub("(.+)\\.(.+)", "\\1", RPT)]
school_topic_anons[,Parent_Topic:=gsub("(.+)\\.(.+)", "\\2", RPT)]



dotsize<-.5
dot<-ggplot(hsr.plot.data,
          aes(x=Parent_Topic, y=School_Anon_N)) + 
  geom_dotplot(data=school_topic_anons, 
               fill="white", 
               color="black", 
               binaxis = "y", 
               dotsize=dotsize,
                ) + 
  geom_dotplot(binaxis = "y", 
               dotsize=dotsize,
               fill="#17345B",
               color="#17345B") +   
  geom_dotplot(data=hsr.plot.data[School=="KIPP Chicago"],
               binaxis = "y", 
               dotsize=dotsize,
               color="orange",
               fill="orange") +   
  facet_grid(.~Role, 
             scales = "free_x") +
  theme_bw() + 
  theme(axis.text.x=element_text(angle=90)) +
  xlab("Topic") + 
  ylab("School (Total Top Performer Topics in parenthesis)") +
  ggtitle("HSR Schoolal Top Performers\nCount of Top Quartile Parent Topics by Role")
dot

pdf("graphs/hsr_dots_alpha.pdf", width=8.5, height=11)
  dot
dev.off()

#ranked by total top quartile ####
school_anon_ranked<-hsr.plot.data[,list(School_Anon_N, N)][order(N)][,unique(School_Anon_N)]
#reorder schools using total top quartile parent topics
school_topic_anons[,School_Anon_N:=factor(School_Anon_N, 
                                          levels=school_anon_ranked)]
hsr.plot.data[,School_Anon_N:=factor(School_Anon_N, 
                                     levels=school_anon_ranked)]

dot.r<-ggplot(hsr.plot.data,
            aes(x=Parent_Topic, y=School_Anon_N)) + 
  geom_dotplot(data=school_topic_anons, 
               fill="white", 
               color="black", 
               binaxis = "y", 
               dotsize=dotsize,
  ) + 
  geom_dotplot(binaxis = "y", 
               dotsize=dotsize,
               fill="#17345B",
               color="#17345B") +     
  geom_dotplot(data=hsr.plot.data[School=="KIPP Chicago"],
               binaxis = "y", 
               dotsize=dotsize,
               color="orange",
               fill="orange") +   
  facet_grid(.~Role, 
             scales = "free_x") +
  theme_bw() + 
  theme(axis.text.x=element_text(angle=90)) +
  xlab("Topic") + 
  ylab("School (Total Top Performer Topics in parenthesis)") +
  ggtitle("HSR Schoolal Top Performers\nCount of Top Quartile Parent Topics by Role")
dot.r

pdf("graphs/hsr_dots_ranked.pdf", width=8.5, height=11)
 dot.r
dev.off()

# Non-anonymous ####
hsr.plot.data[,School_N:=paste(School, "(", N, ")")]


school_N_ranked<-hsr.plot.data[,list(School_N, N)][order(N)][,unique(School_N)]
#reorder schools using total top quartile parent topics

school_topic_N<-data.table(expand.grid(School_N=unique(hsr.plot.data$School_N), RPT=ptopics$RPT))


#extrat roles and parent topics from rpt
school_topic_N[,Role:=gsub("(.+)\\.(.+)", "\\1", RPT)]
school_topic_N[,Parent_Topic:=gsub("(.+)\\.(.+)", "\\2", RPT)]


school_topic_N[,School_N:=factor(School_N, 
                                          levels=school_N_ranked)]


hsr.plot.data[,School_Anon_N:=factor(School_Anon_N, 
                                     levels=school_anon_ranked)]

dot.r.named<-ggplot(hsr.plot.data,
              aes(x=Parent_Topic, y=School_N)) + 
  geom_dotplot(data=school_topic_N, 
               fill="white", 
               color="black", 
               binaxis = "y", 
               dotsize=dotsize,
  ) + 
  geom_dotplot(binaxis = "y", 
               dotsize=dotsize,
               fill="#17345B",
               color="#17345B") +   
  geom_dotplot(data=hsr.plot.data[School %in% c("KIPP Create Middle School",
                                                "KIPP Ascend Middle School",
                                                "KIPP Bloom College Prep")],
               binaxis = "y", 
               dotsize=dotsize,
               color="orange",
               fill="orange") +   
  facet_grid(.~Role, 
             scales = "free_x") +
  theme_bw() + 
  theme(axis.text.x=element_text(angle=90)) +
  xlab("Topic") + 
  ylab("School (Total Top Performer Topics in parenthesis)") +
  ggtitle("HSR Schools Top Performers\nCount of Top Quartile Parent Topics by Role")
dot.r.named

pdf("graphs/hsr_dots_schools_named_ranked.pdf", width=8.5, height=11)
dot.r.named
dev.off()