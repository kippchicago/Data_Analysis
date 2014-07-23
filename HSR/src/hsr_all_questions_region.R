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

hsr.avg.chinat[, Role2:=factor(Role, levels=c("Student",
                                             "Parent",
                                             "Teacher",
                                             "School Leader",
                                             "Staff"
                                             )
                              )
                ]


p.avg<-ggplot(hsr.avg.chinat, 
              aes(y=Survey.Question, x=factor(0)
                  )
              ) + 
  geom_point(aes(color=Magnitude),
             size=1.5) +
  scale_color_gradientn(colours=c("red", "gray", "green"),
                      values=rescale(c(min(hsr.avg.chinat$Magnitude), 1, max(hsr.avg.chinat$Magnitude)))
                       ) +
  facet_grid(Parent.Topic~Role2,
             scale="free", 
             space = "free",
             drop = T
             ) + 
  theme_bw() + 
  guides(color="none", 
         shape="none") +
  theme(axis.text.y=element_text(size=5),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        strip.text=element_text(size=6)
        ) + 
  ylab("") + 
  xlab("")

pdf("graphs/hsr_questions.pdf", height=11, width=8.5)
p.avg
dev.off()



# Two pager ####

colorscale<-rescale(c(min(hsr.avg.chinat$Magnitude), 
                      1, 
                      max(hsr.avg.chinat$Magnitude)))

p.avg2<-ggplot(hsr.avg.chinat, 
              aes(y=Survey.Question, x=factor(0)
              )
) + 
  geom_point(aes(color=Magnitude,
                 shape=Magnitude<1),
             size=2) +
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
  theme(axis.text.y=element_text(size=6),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        strip.text=element_text(size=6)
  ) + 
  ylab("") + 
  xlab("")


#hsr.page1<-hsr.avg.chinat[Parent.Topic %in% c("Attainment", "Character", "Culture and Climate")]
#hsr.page2<-hsr.avg.chinat[!(Parent.Topic %in% c("Attainment", "Character", "Culture and Climate"))]

pdf("graphs/hsr_questions_2pager.pdf", height=11, width=8.5)
p.avg2 %+% hsr.avg.chinat[Parent.Topic %in% c("Attainment", "Character", "Culture and Climate")]
p.avg2 %+% hsr.avg.chinat[!(Parent.Topic %in% c("Attainment", "Character", "Culture and Climate"))]
dev.off()

# now with raw scores added
p.avg3<-ggplot(hsr.avg.chinat, 
               aes(y=Survey.Question, x=factor(0)
               )
) + 
  geom_point(aes(color=Magnitude,
                 shape=Magnitude<1),
             size=2) +
  geom_text(aes(x=.90,
                label=KIPP.Chicago), 
            size=1.75,
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
  theme(axis.text.y=element_text(size=6),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        strip.text=element_text(size=6)
  ) + 
  ylab("") + 
  xlab("")


#hsr.page1<-hsr.avg.chinat[Parent.Topic %in% c("Attainment", "Character", "Culture and Climate")]
#hsr.page2<-hsr.avg.chinat[!(Parent.Topic %in% c("Attainment", "Character", "Culture and Climate"))]

pdf("graphs/hsr_questions_2pager.pdf", height=11, width=8.5)
p.avg3 %+% hsr.avg.chinat[Parent.Topic %in% c("Attainment", "Character", "Culture and Climate")]
p.avg3 %+% hsr.avg.chinat[!(Parent.Topic %in% c("Attainment", "Character", "Culture and Climate"))]
dev.off()

# KAP 2 pager ###

hsr.kap <- prepSurveyAvg(hsr.all, school = "KIPP Ascend Primary School")
setnames(hsr.kap, "KAP", "KIPP.Chicago")
pdf("graphs/hsr_questions_2pager_KAP.pdf", height=11, width=8.5)
p.avg3 %+% 
  hsr.kap[Parent.Topic %in% c("Attainment", "Character", "Culture and Climate")] + 
  ggtitle("KAP")
p.avg3 %+% hsr.kap[!(Parent.Topic %in% c("Attainment", "Character", "Culture and Climate"))] +
  ggtitle("KAP")
dev.off()

# KAMS 2 pager ###

hsr.kams <- prepSurveyAvg(hsr.all, school = "KIPP Ascend Middle School")
setnames(hsr.kams, "KAMS", "KIPP.Chicago")
pdf("graphs/hsr_questions_2pager_KAMS_2.pdf", height=11, width=8.5)
p.avg3 %+% 
  hsr.kams[Parent.Topic %in% c("Attainment", "Character", "Culture and Climate")] + 
  ggtitle("KAMS | All 203 HSR Questions\nPage 1 of 2")
p.avg3 %+% hsr.kams[!(Parent.Topic %in% c("Attainment", "Character", "Culture and Climate"))] +
  ggtitle("KAMS | All 203 HSR Questions\nPage 2 of 2")
dev.off()

# KCCP 2 pager ###

hsr.kccp <- prepSurveyAvg(hsr.all, school = "KIPP Create Middle School")
setnames(hsr.kccp, "KCCP", "KIPP.Chicago")
pdf("graphs/hsr_questions_2pager_KCCP.pdf", height=11, width=8.5)
p.avg3 %+% 
  hsr.kccp[Parent.Topic %in% c("Attainment", "Character", "Culture and Climate")] + 
  ggtitle("KCCP")
p.avg3 %+% hsr.kccp[!(Parent.Topic %in% c("Attainment", "Character", "Culture and Climate"))] +
  ggtitle("KCCP")
dev.off()

# KBCP 2 pager ###

hsr.kccp <- prepSurveyAvg(hsr.all, school = "KIPP Bloom College Prep")
setnames(hsr.kccp, "KBCP", "KIPP.Chicago")
pdf("graphs/hsr_questions_2pager_KBCP.pdf", height=11, width=8.5)
p.avg3 %+% 
  hsr.kccp[Parent.Topic %in% c("Attainment", "Character", "Culture and Climate")] + 
  ggtitle("KBCP")
p.avg3 %+% hsr.kccp[!(Parent.Topic %in% c("Attainment", "Character", "Culture and Climate"))] +
  ggtitle("KBCP")
dev.off()
  