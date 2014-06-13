library('ProjectTemplate')
load.project()


#subset lickert measures
lick.levels<-c("Strongly Disagree",
               "Disagree",
               "Neutral",
               "Agree",
               "Strongly Agree")

hsr.lickert<-hsr[Measure.Names %in% lick.levels]
hsr.lickert[, Measure.Names:=factor(Measure.Names, levels=lick.levels, ordered=TRUE)]

#identify center level
lick.names.len<-length(levels(hsr.lickert$Measure.Names))
center<-(lick.names.len-1)/2 + 1 
center.name<-levels(hsr.lickert$Measure.Names)[center]
neg.names <- levels(hsr.lickert$Measure.Names)[1:(center-1)]
pos.names <- levels(hsr.lickert$Measure.Names)[(center+1):lick.names.len]

neu<-hsr.lickert[Measure.Names %in% center.name]
pos<-hsr.lickert[Measure.Names %in% pos.names]
neg<-hsr.lickert[Measure.Names %in% neg.names]

neg<-neg[order(Measure.Names, decreasing = T)]
neg[,x:=cumsum(Measure.Values)-Measure.Values, by=list(School, Survey.Question)]
neg[,xend:=x+Measure.Values]
neg[,x:=-x]
neg[,xend:=-xend]

pos<-pos[order(Measure.Names)]
pos[,x:=cumsum(Measure.Values)-Measure.Values, by=list(School, Survey.Question)]
pos[,xend:=x+Measure.Values]

neu[,x:=-Measure.Values/2]
neu[,xend:=-x]

nn<-left_join(x=neg, y=neu[,list(School, Survey.Question, adj=x)], by = c("School", "Survey.Question"))
nn<-data.table(nn)
nn[,x:=x+adj]
nn[,xend:=xend+adj]
nn[,adj:=NULL]
neg<-nn

pn<-left_join(x=pos, y=neu[,list(School, Survey.Question, adj=xend)], by = c("School", "Survey.Question"))
pn<-data.table(pn)
pn[,x:=x+adj]
pn[,xend:=xend+adj]
pn[,adj:=NULL]
pos<-pn

rm(nn)
rm(pn)

hsr.plot.data<-rbind(pos, neg, neu)
hsr.plot.data[,Measure.Names:=factor(as.character(Measure.Names), levels=lick.levels, ordered=TRUE)]
hsr.plot.data[,School:=abbrev(School, exceptions=list(old=c("KAPS", "KCMS"), new=c("KAP", "KCCP")))]
hsr.plot.data[,School:=factor(School, levels=rev(c("KAP", "KAMS", "KCCP", "KBCP", "KC", "KN")))]

hsr.plot.data[grepl("Agree", Measure.Names),Sign:="Strongly Agree"]
hsr.plot.data[grepl("Disagree", Measure.Names),Sign:="Strongly Disagree"]
hsr.plot.data[grepl("Neutral", Measure.Names),Sign:="Neutral"]
hsr.plot.data[,SQ:=str_wrap(Survey.Question, width=90)]

hsr.sum.pos.neg<-hsr.plot.data[,list(Sum=sum(Measure.Values)), 
                               by=list(Topic.Name, 
                                       SQ, 
                                       School, Sign)]
hsr.sum.pos.neg[Sign=="Strongly Agree", x:=1.05]
hsr.sum.pos.neg[Sign=="Strongly Disagree", x:=-0.85]
hsr.sum.pos.neg[Sign=="Neutral", x:=0]

topics<-hsr.plot.data[,unique(Topic.Name)]





topic.list<-c("Instructional Strategies", "Instructional Leadership", "Instructional Planning")


p<-list()
p$mcs<-plot_lickert(hsr.plot.data, "Motivation, Commitment, and Satisfaction")

p$ve_re<-plot_lickert(hsr.plot.data, c("Values and Expectations",
                        "Results orientation"))

p$c_sap<-plot_lickert(hsr.plot.data, c("Curriculum",
                        "Student academic preparedness"))

p$spm_srr_se<-plot_lickert(hsr.plot.data, c("Student academic preparedness",
                             "Staff recruitment and retention",
                             "School Environment"))

p$inspl_instrl_dl<-plot_lickert(hsr.plot.data, c("Inspirational Leadership",
                                  "Instructional Leadership",
                                  "Distributed leadership"))

p$is_ip<-plot_lickert(hsr.plot.data, c("Instructional Strategies",
                        "Instructional Planning"))

p$sbm_lbdd_fe_di<-plot_lickert(hsr.plot.data, c("Student Behavior Management",
                                 "Leadership bench depth and development",
                                 "Family engagement",
                                 "Diversity and inclusivity"))



pdf("graphs/hsr_2.pdf", height=11, width=8.5)
p
dev.off()

# KAMS ####

schoollist<-c("KAMS", "KC", "KN")
p<-list()
p$mcs<-plot_lickert(hsr.plot.data[School %in% schoollist], "Motivation, Commitment, and Satisfaction")

p$ve_re<-plot_lickert(hsr.plot.data[School %in% schoollist], c("Values and Expectations",
                                       "Results orientation"))

p$c_sap<-plot_lickert(hsr.plot.data[School %in% schoollist], c("Curriculum",
                                       "Student academic preparedness"))

p$spm_srr_se<-plot_lickert(hsr.plot.data[School %in% schoollist], c("Student academic preparedness",
                                            "Staff recruitment and retention",
                                            "School Environment"))

p$inspl_instrl_dl<-plot_lickert(hsr.plot.data[School %in% schoollist], c("Inspirational Leadership",
                                                 "Instructional Leadership",
                                                 "Distributed leadership"))

p$is_ip<-plot_lickert(hsr.plot.data[School %in% schoollist], c("Instructional Strategies",
                                       "Instructional Planning"))

p$sbm_lbdd_fe_di<-plot_lickert(hsr.plot.data[School %in% schoollist], c("Student Behavior Management",
                                                "Leadership bench depth and development",
                                                "Family engagement",
                                                "Diversity and inclusivity"))



pdf("graphs/hsr_KAMS.pdf", height=11, width=8.5)
p
dev.off()

# KBCP ####

schoollist<-c("KBCP", "KC", "KN")
p<-list()
p$mcs<-plot_lickert(hsr.plot.data[School %in% schoollist], "Motivation, Commitment, and Satisfaction")

p$ve_re<-plot_lickert(hsr.plot.data[School %in% schoollist], c("Values and Expectations",
                                                               "Results orientation"))

p$c_sap<-plot_lickert(hsr.plot.data[School %in% schoollist], c("Curriculum",
                                                               "Student academic preparedness"))

p$spm_srr_se<-plot_lickert(hsr.plot.data[School %in% schoollist], c("Student academic preparedness",
                                                                    "Staff recruitment and retention",
                                                                    "School Environment"))

p$inspl_instrl_dl<-plot_lickert(hsr.plot.data[School %in% schoollist], c("Inspirational Leadership",
                                                                         "Instructional Leadership",
                                                                         "Distributed leadership"))

p$is_ip<-plot_lickert(hsr.plot.data[School %in% schoollist], c("Instructional Strategies",
                                                               "Instructional Planning"))

p$sbm_lbdd_fe_di<-plot_lickert(hsr.plot.data[School %in% schoollist], c("Student Behavior Management",
                                                                        "Leadership bench depth and development",
                                                                        "Family engagement",
                                                                        "Diversity and inclusivity"))



pdf("graphs/hsr_KBCP.pdf", height=11, width=8.5)
p
dev.off()

# KAP ####

schoollist<-c("KAP", "KC", "KN")
p<-list()
p$mcs<-plot_lickert(hsr.plot.data[School %in% schoollist], "Motivation, Commitment, and Satisfaction")

p$ve_re<-plot_lickert(hsr.plot.data[School %in% schoollist], c("Values and Expectations",
                                                               "Results orientation"))

p$c_sap<-plot_lickert(hsr.plot.data[School %in% schoollist], c("Curriculum",
                                                               "Student academic preparedness"))

p$spm_srr_se<-plot_lickert(hsr.plot.data[School %in% schoollist], c("Student academic preparedness",
                                                                    "Staff recruitment and retention",
                                                                    "School Environment"))

p$inspl_instrl_dl<-plot_lickert(hsr.plot.data[School %in% schoollist], c("Inspirational Leadership",
                                                                         "Instructional Leadership",
                                                                         "Distributed leadership"))

p$is_ip<-plot_lickert(hsr.plot.data[School %in% schoollist], c("Instructional Strategies",
                                                               "Instructional Planning"))

p$sbm_lbdd_fe_di<-plot_lickert(hsr.plot.data[School %in% schoollist], c("Student Behavior Management",
                                                                        "Leadership bench depth and development",
                                                                        "Family engagement",
                                                                        "Diversity and inclusivity"))



pdf("graphs/hsr_KAP.pdf", height=11, width=8.5)
p
dev.off()


# Region only ####

schoollist<-c("KC", "KN")

p<-list()
p$mcs<-plot_lickert(hsr.plot.data[School %in% schoollist], "Motivation, Commitment, and Satisfaction")

p$ve_re<-plot_lickert(hsr.plot.data[School %in% schoollist], c("Values and Expectations",
                                                               "Results orientation"))

p$c_sap<-plot_lickert(hsr.plot.data[School %in% schoollist], c("Curriculum",
                                                               "Student academic preparedness"))

p$spm_srr_se<-plot_lickert(hsr.plot.data[School %in% schoollist], c("Student academic preparedness",
                                                                    "Staff recruitment and retention",
                                                                    "School Environment"))

p$inspl_instrl_dl<-plot_lickert(hsr.plot.data[School %in% schoollist], c("Inspirational Leadership",
                                                                         "Instructional Leadership",
                                                                         "Distributed leadership"))

p$is_ip<-plot_lickert(hsr.plot.data[School %in% schoollist], c("Instructional Strategies",
                                                               "Instructional Planning"))

p$sbm_lbdd_fe_di<-plot_lickert(hsr.plot.data[School %in% schoollist], c("Student Behavior Management",
                                                                        "Leadership bench depth and development",
                                                                        "Family engagement",
                                                                        "Diversity and inclusivity"))



pdf("graphs/hsr_Region.pdf", height=11, width=8.5)
p
dev.off()


hsr.lickert<-setkey(hsr.lickert, Survey.Question, Measure.Names)

hsr.regnet<-copy(hsr.lickert[School=="KIPP Chicago"][hsr.lickert[School=="KIPP Network"]])

assert_that(nrow(hsr.regnet)==nrow(hsr.lickert[School=="KIPP Chicago"]))
assert_that(nrow(hsr.regnet)==nrow(hsr.lickert[School=="KIPP Network"]))

hsr.regnet[,Positive:=ifelse(Measure.Names %in% c("Agree", "Strongly Agree"), TRUE, FALSE)]

hsr.regnet[Positive==TRUE,
           list(Chicago=sum(Measure.Values), 
                Network=sum(Measure.Values.1)
                ), 
           by=list(Topic.Name, Survey.Question
                   )
           ][order(Topic.Name)][,list(Pct=sum(Chicago>=Network)/.N, 
                                      Pct_Chi=sum(Chicago>=.5)/.N,
                                      Pct_Network=sum(Network>=.5)/.N)]




