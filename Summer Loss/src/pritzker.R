# Script to create single (per subject) seagull for KIPP Network and KIPP Chicaog.
setwd("~/Dropbox/Consulting/KIPP Ascend/Data Analysis/Summer Loss")
require(ProjectTemplate)
load.project()

# 7th grade Only ####



kipp.7<-kipp.dt[Grade==7 & Subtest.Name%in% c("MAP.Mathematics", "MAP.Reading")]

kipp.7<-kipp.7[!(is.na(S2012_Avg.RIT.Score)|
           is.na(F2012_Avg.RIT.Score)|
           is.na(S2013_Avg.RIT.Score))]

kipp.7.chi<-kipp.dt[KIPP.Region=="KIPP Chicago" & Grade==7 & Subtest.Name%in% c("MAP.Mathematics", "MAP.Reading")]



kipp.7

# Calc weighted average for KIPP Network for each season. 

kipp.7.avg<-kipp.7[, list(
              S2012_Avg=sum(S2012_Avg.RIT.Score*S2012_N/sum(S2012_N, na.rm=TRUE), na.rm=TRUE),
             F2012_Avg=sum(F2012_Avg.RIT.Score*F2012_N/sum(F2012_N, na.rm=TRUE), na.rm=TRUE),
             S2013_Avg=sum(S2013_Avg.RIT.Score*S2013_N/sum(S2013_N, na.rm=TRUE), na.rm=TRUE)
             ),
             by=Subtest.Name]


kipp.7.avg[,KIPP.Region:="KIPP Network"]

kipp.7.chi[,c("School.ID", "School", "Grade", "S2012_N", "F2012_N", "S2013_N"):=NULL]

setnames(kipp.7.chi,
         c("S2012_Avg.RIT.Score",
              "F2012_Avg.RIT.Score",
              "S2013_Avg.RIT.Score"),
         c("S2012_Avg",
              "F2012_Avg",
              "S2013_Avg"))
kipp.7.chi[,KIPP.Region:=as.character("KIPP Chicago")]
kipp.7.combined<-rbind(kipp.7.avg, kipp.7.chi)

setnames(kipp.7.combined, c("Subject", 
                            "Spring 2012", 
                            "Fall 2012", 
                            "Spring 2013", 
                            "Region"))
kipp.7.combined[,Subject:=gsub("(^MAP\\.)", "", Subject)]

kipp.7.combined
kipp.7.melt<-melt(kipp.7.combined, id.vars = c("Subject", "Region"))

kipp.7.melt$variable<-factor(kipp.7.melt$variable, 
                             levels=c("Spring 2012",
                                      "Fall 2012",
                                      "Spring 2013")
                             )

kipp.7.melt<-data.table(kipp.7.melt)

kipp.7.melt[,Region:=factor(Region, levels=c("KIPP Network", "KIPP Chicago"))]

p.7<-ggplot(kipp.7.melt, aes(x=variable, y=value)) + 
  geom_line(aes(group=Region, color=Region), 
            linetype=3) +
  geom_line(data=kipp.7.melt[variable!="Fall 2012"],
            aes(group=Region, color=Region), 
            linetype=1,
            size=1.5) +
  scale_color_manual(values = c("gray", "#255694")) +
  facet_grid(Subject~., scales = "free_y") +
  xlab("Test Term") +
  ylab("RIT Score") +
  theme_bw()

p.7

# Grades 1-3 and 6-8 ####
kipp.all<-kipp.dt[Grade %in% c(1:2, 6:8) & Subtest.Name%in% c("MAP.Mathematics", "MAP.Reading")]

kipp.all<-kipp.all[!(is.na(S2012_Avg.RIT.Score)|
                   is.na(F2012_Avg.RIT.Score)|
                   is.na(S2013_Avg.RIT.Score))]

kipp.all.chi<-kipp.all[KIPP.Region=="KIPP Chicago"]



# Calc weighted average for KIPP Network for each season. 

kipp.all.avg<-kipp.all[, list(
  S2012_Avg=sum(S2012_Avg.RIT.Score*S2012_N/sum(S2012_N, na.rm=TRUE), na.rm=TRUE),
  F2012_Avg=sum(F2012_Avg.RIT.Score*F2012_N/sum(F2012_N, na.rm=TRUE), na.rm=TRUE),
  S2013_Avg=sum(S2013_Avg.RIT.Score*S2013_N/sum(S2013_N, na.rm=TRUE), na.rm=TRUE)
),
by=Subtest.Name]


kipp.all.avg[,KIPP.Region:="KIPP Network"]

kipp.all.chi.avg<-kipp.all.chi[, list(
  S2012_Avg=sum(S2012_Avg.RIT.Score*S2012_N/sum(S2012_N, na.rm=TRUE), na.rm=TRUE),
  F2012_Avg=sum(F2012_Avg.RIT.Score*F2012_N/sum(F2012_N, na.rm=TRUE), na.rm=TRUE),
  S2013_Avg=sum(S2013_Avg.RIT.Score*S2013_N/sum(S2013_N, na.rm=TRUE), na.rm=TRUE)
),
by=Subtest.Name]

#kipp.all.chi[,c("School.ID", "School", "Grade", "S2012_N", "F2012_N", "S2013_N"):=NULL]




#setnames(kipp.all.chi.avg,
#         c("S2012_Avg.RIT.Score",
#           "F2012_Avg.RIT.Score",
#           "S2013_Avg.RIT.Score"),
#         c("S2012_Avg",
#           "F2012_Avg",
#           "S2013_Avg"))
kipp.all.chi.avg[,KIPP.Region:=as.character("KIPP Chicago")]
kipp.all.combined<-rbind(kipp.all.avg, kipp.all.chi.avg)

setnames(kipp.all.combined, c("Subject", 
                            "Spring 2012", 
                            "Fall 2012", 
                            "Spring 2013", 
                            "Region"))
kipp.all.combined[,Subject:=gsub("(^MAP\\.)", "", Subject)]

kipp.all.combined
kipp.all.melt<-melt(kipp.all.combined, id.vars = c("Subject", "Region"))

kipp.all.melt$variable<-factor(kipp.all.melt$variable, 
                             levels=c("Spring 2012",
                                      "Fall 2012",
                                      "Spring 2013")
)

kipp.all.melt<-data.table(kipp.all.melt)

kipp.all.melt[,Region:=factor(Region, levels=c("KIPP Network", "KIPP Chicago"))]

p.all<-ggplot(kipp.all.melt, aes(x=variable, y=value)) + 
  geom_line(aes(group=Region, color=Region), 
            linetype=3) +
  geom_line(data=kipp.all.melt[variable!="Fall 2012"],
            aes(group=Region, color=Region), 
            linetype=1,
            size=1.5) +
  scale_color_manual(values = c("gray", "#255694")) +
  facet_grid(Subject~., scales = "free_y") +
  xlab("Test Term") +
  ylab("RIT Score") +
  theme_bw()

png('graphs/pritz_all_grades.png', height=356, width=624, units="px")
p.all
dev.off()

# KIPP Chicago Only, 6-8 Reading  ####
kipp.68<-kipp.dt[Grade %in% c(6:8) & Subtest.Name%in% c( "MAP.Reading")]

kipp.68<-kipp.68[!(is.na(S2012_Avg.RIT.Score)|
                       is.na(F2012_Avg.RIT.Score)|
                       is.na(S2013_Avg.RIT.Score))]

kipp.68.chi<-kipp.68[KIPP.Region=="KIPP Chicago"]



# Calc weighted average for KIPP Network for each season. 

kipp.68.avg<-kipp.68[, list(
  S2012_Avg=sum(S2012_Avg.RIT.Score*S2012_N/sum(S2012_N, na.rm=TRUE), na.rm=TRUE),
  F2012_Avg=sum(F2012_Avg.RIT.Score*F2012_N/sum(F2012_N, na.rm=TRUE), na.rm=TRUE),
  S2013_Avg=sum(S2013_Avg.RIT.Score*S2013_N/sum(S2013_N, na.rm=TRUE), na.rm=TRUE)
),
by=Subtest.Name]


kipp.68.avg[,KIPP.Region:="KIPP Network"]

kipp.68.chi.avg<-kipp.68.chi[, list(
  S2012_Avg=sum(S2012_Avg.RIT.Score*S2012_N/sum(S2012_N, na.rm=TRUE), na.rm=TRUE),
  F2012_Avg=sum(F2012_Avg.RIT.Score*F2012_N/sum(F2012_N, na.rm=TRUE), na.rm=TRUE),
  S2013_Avg=sum(S2013_Avg.RIT.Score*S2013_N/sum(S2013_N, na.rm=TRUE), na.rm=TRUE)
),
by=Subtest.Name]


kipp.68.chi.avg[,KIPP.Region:=as.character("KIPP Chicago")]
kipp.68.combined<-rbind(kipp.68.avg, kipp.68.chi.avg)

setnames(kipp.68.combined, c("Subject", 
                              "Spring 2012", 
                              "Fall 2012", 
                              "Spring 2013", 
                              "Region"))
kipp.68.combined[,Subject:=gsub("(^MAP\\.)", "", Subject)]

kipp.68.combined
kipp.68.melt<-melt(kipp.68.combined, id.vars = c("Subject", "Region"))

kipp.68.melt$variable<-factor(kipp.68.melt$variable, 
                               levels=c("Spring 2012",
                                        "Fall 2012",
                                        "Spring 2013")
)

kipp.68.melt<-data.table(kipp.68.melt)

kipp.68.melt[,Region:=factor(Region, levels=c("KIPP Network", "KIPP Chicago"))]

p.68<-ggplot(kipp.68.melt[Region=="KIPP Chicago"], aes(x=variable, y=value)) + 
  geom_line(aes(group=Region, color=Region), 
            linetype=3) +
  geom_line(data=kipp.68.melt[Region == "KIPP Chicago" & variable!="Fall 2012"],
            aes(group=Region, color=Region), 
            linetype=1,
            size=1.5) +
  scale_color_manual(values = c("#255694")) +
  facet_grid(Subject~., scales = "free_y") +
  xlab("Test Term") +
  ylab("RIT Score") +
  theme_bw()

p.68

png('graphs/sl_pritz_68.png', height=356, width=624, units="px")
p.68
dev.off()


# 5th Grade Average RIT Ranked
k.5.dt<-AvgRITbySchool.2014.04.01[Grade=="5th Grade" & Subtest.Name %in% c("MAP.Mathematics", "MAP.Reading")]
k.5.dt[, Rank:=rank(-F2012_Avg.RIT.Score, ties.method = "first"), by=Subtest.Name]

k.5.dt<-k.5.dt[,list(Subject=Subtest.Name,
                     School,
                     Region=KIPP.Region,
                     Grade,
                     F12_Avg_RIT=F2012_Avg.RIT.Score,
                     Rank)]

k.5.dt[,Subject:=gsub("(^MAP\\.)", "", Subject)]

k.5.dt[School=="KIPP Create Middle School", School:="KIPP Create College Prep"]


p.FallRITRank <- ggplot(k.5.dt, aes(x=F12_Avg_RIT, y= -Rank)) +
  geom_point(color="grey", size=3)+
  geom_point(data=k.5.dt[Region=="KIPP Chicago"], 
             color="forestgreen",
             size=3) +
  geom_text(data=k.5.dt[Region=="KIPP Chicago"], 
            aes(x= F12_Avg_RIT + 1, 
                label=School), 
            size=3, 
            hjust=0, 
            color="forestgreen") +
  facet_grid(. ~ Subject) + 
  
  theme_bw() + theme(axis.text.y=element_blank(),
                     axis.ticks.y=element_blank()
                     ) + 
  xlab("Fall 2012 Average RIT Score") +
  ylab("Rank")

p.FallRITRank

png('graphs/sl_pritz_fall_ranks.png', height=700, width=1200, units="px")
p.FallRITRank
dev.off()

pdf('graphs/sl_pritz_fall_ranks.pdf', height=8.25, width=10.75)
p.FallRITRank
dev.off()

pdf('graphs/sl_pritz_68.pdf', height=8.25, width=10.75)
p.68
dev.off()

p.68.all<-ggplot(kipp.68.melt, aes(x=variable, y=value)) + 
  geom_line(aes(group=Region, color=Region), 
            linetype=3) +
  geom_line(data=kipp.68.melt[variable!="Fall 2012"],
            aes(group=Region, color=Region), 
            linetype=1,
            size=1.5) +
  scale_color_manual(values = c("gray","#255694")) +
  facet_grid(Subject~., scales = "free_y") +
  xlab("Test Term") +
  ylab("RIT Score") +
  theme_bw()
