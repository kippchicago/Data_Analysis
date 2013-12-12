library(ggplot2)
library(gridExtra)
library(plyr)
library(reshape)


Q12<-read.csv('/Users/chaid/Dropbox/Consulting/KIPP Ascend/Data Analysis/Q12/Q12_All_Responces_121113.csv')


Q12<-Q12[,c(1,5:16)]

quests<-names(Q12)[-1]


for(n in quests){
  Q12[,n] <- factor(x=Q12[,n], levels=c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"))
  Q12[,n] <- ordered(x=Q12[,n])
}


Q12.list<-as.list(Q12[,-1])
for(i in 1:length(Q12.list)){
  Q12.list[[i]]<-Q12.list[[i]][!is.na(Q12.list[[i]])]
  levels(Q12.list[[i]])<-c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
}

#Change list attributes to full question statements (yes, that is oxymoronic)
names(Q12.list)<-c(
                  "1. I know what is expected of me at work.",
                  "2. I have the materials and equipment needed to do my work.",
                  "3. At work I have the opportunity to do what I do best everyday.",
                  "4. In the last seven days I have received praise or recognition at work.",
                  "5. My supervisor, or someone at work, cares about me as a person.",
                  "6. Someone at work encourages my development.",
                  "7. At work, my opinions seem to matter.",
                  "8. The mission of my organization makes me feel like my job is important.",
                  "9. My coworkers are committed to doing high-quality work.",
                  "10. I have a best friend at work.",
                  "11. In the last six months someone at work has talked to me about my progress.",
                  "12. In the last year I have had opportunities to learn and grow.")
Q12.graph<-net_stacked(Q12.list)
Q12.graph



##Same thing but by school
#KAPS
Q12.KAPS.list<-as.list(subset(Q12,School=="KAP")[,-1])
for(i in 1:length(Q12.KAPS.list)){
  Q12.KAPS.list[[i]]<-Q12.KAPS.list[[i]][!is.na(Q12.KAPS.list[[i]])]
  levels(Q12.KAPS.list[[i]])<-c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
}

names(Q12.KAPS.list)<-c(
  "1. I know what is expected of me at work.",
  "2. I have the materials and equipment needed to do my work.",
  "3. At work I have the opportunity to do what I do best everyday.",
  "4. In the last seven days I have received praise or recognition at work.",
  "5. My supervisor, or someone at work, cares about me as a person.",
  "6. Someone at work encourages my development.",
  "7. At work, my opinions seem to matter.",
  "8. The mission of my organization makes me feel like my job is important.",
  "9. My coworkers are committed to doing high-quality work.",
  "10. I have a best friend at work.",
  "11. In the last six months someone at work has talked to me about my progress.",
  "12. In the last year I have had opportunities to learn and grow.")

net_stacked(Q12.KAPS.list)

#KAMS
Q12.KAMS.list<-as.list(subset(Q12,School=="KAMS")[,-1])
for(i in 1:length(Q12.KAMS.list)){
  Q12.KAMS.list[[i]]<-Q12.KAMS.list[[i]][!is.na(Q12.KAMS.list[[i]])]
  levels(Q12.KAMS.list[[i]])<-c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
}

names(Q12.KAMS.list)<-c(
  "1. I know what is expected of me at work.",
  "2. I have the materials and equipment needed to do my work.",
  "3. At work I have the opportunity to do what I do best everyday.",
  "4. In the last seven days I have received praise or recognition at work.",
  "5. My supervisor, or someone at work, cares about me as a person.",
  "6. Someone at work encourages my development.",
  "7. At work, my opinions seem to matter.",
  "8. The mission of my organization makes me feel like my job is important.",
  "9. My coworkers are committed to doing high-quality work.",
  "10. I have a best friend at work.",
  "11. In the last six months someone at work has talked to me about my progress.",
  "12. In the last year I have had opportunities to learn and grow.")

net_stacked(Q12.KAMS.list)


#KCCP
Q12.KCCP.list<-as.list(subset(Q12,School=="KCCP")[,-1])
for(i in 1:length(Q12.KCCP.list)){
  Q12.KCCP.list[[i]]<-Q12.KCCP.list[[i]][!is.na(Q12.KCCP.list[[i]])]
  levels(Q12.KCCP.list[[i]])<-c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
}

names(Q12.KCCP.list)<-c(
  "1. I know what is expected of me at work.",
  "2. I have the materials and equipment needed to do my work.",
  "3. At work I have the opportunity to do what I do best everyday.",
  "4. In the last seven days I have received praise or recognition at work.",
  "5. My supervisor, or someone at work, cares about me as a person.",
  "6. Someone at work encourages my development.",
  "7. At work, my opinions seem to matter.",
  "8. The mission of my organization makes me feel like my job is important.",
  "9. My coworkers are committed to doing high-quality work.",
  "10. I have a best friend at work.",
  "11. In the last six months someone at work has talked to me about my progress.",
  "12. In the last year I have had opportunities to learn and grow.")

#KCCP
Q12.KBCP.list<-as.list(subset(Q12,School=="KBCP")[,-1])
for(i in 1:length(Q12.KBCP.list)){
  Q12.KBCP.list[[i]]<-Q12.KBCP.list[[i]][!is.na(Q12.KBCP.list[[i]])]
  levels(Q12.KBCP.list[[i]])<-c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
}

names(Q12.KBCP.list)<-c(
  "1. I know what is expected of me at work.",
  "2. I have the materials and equipment needed to do my work.",
  "3. At work I have the opportunity to do what I do best everyday.",
  "4. In the last seven days I have received praise or recognition at work.",
  "5. My supervisor, or someone at work, cares about me as a person.",
  "6. Someone at work encourages my development.",
  "7. At work, my opinions seem to matter.",
  "8. The mission of my organization makes me feel like my job is important.",
  "9. My coworkers are committed to doing high-quality work.",
  "10. I have a best friend at work.",
  "11. In the last six months someone at work has talked to me about my progress.",
  "12. In the last year I have had opportunities to learn and grow.")

net_stacked(Q12.KBCP.list)


net_stacked(Q12.list)

t1<-textGrob("KIPP Chicago")
t2<-textGrob("KIPP Ascend Primary School")
t3<-textGrob("KIPP Ascend Middle School")
t4<-textGrob("KIPP Create College Prep")
t5<-textGrob("KIPP Bloom College Prep")


tmp <- ggplot_gtable(ggplot_build(net_stacked(Q12.KBCP.list)))
leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
legend <- tmp$grobs[[leg]]


#Plot each separately. 
grid.arrange(arrangeGrob(t1, net_stacked(Q12.list)+theme(legend.position="none"), t2, net_stacked(Q12.KAMS.list)+theme(legend.position="none"), t3, net_stacked(Q12.KAPS.list)+theme(legend.position="none"), t4, net_stacked(Q12.KCCP.list)+theme(legend.position="none"), t5, net_stacked(Q12.KBCP.list)+theme(legend.position="none"), ncol=1, heights=c(.5,4,.5,4,.5,4,.5,4)), legend, widths=unit.c(unit(1, "npc") - legend$widths[2], legend$widths[2]),  ncol=2)

pdf(file='graphs/Q12_Fall13_All_Schools.pdf', width=11, height=8.5)
  grid.arrange(arrangeGrob(t1, 
                           net_stacked(Q12.list)+theme(legend.position="none", 
                                                       axis.text=element_text(size=7)), 
                           t2, 
                           net_stacked(Q12.KAMS.list)+theme(legend.position="none", 
                                                            axis.text=element_text(size=7)), 
                           t3, 
                           net_stacked(Q12.KAPS.list)+theme(legend.position="none", 
                                                            axis.text=element_text(size=7)), 
                           t4, 
                           net_stacked(Q12.KCCP.list)+theme(legend.position="none", 
                                                            axis.text=element_text(size=7)), 
                           t5, 
                           net_stacked(Q12.KBCP.list)+theme(legend.position="none", 
                                                            axis.text=element_text(size=7)), 
                           ncol=1, 
                           heights=c(.5,4,.5,4,.5,4,.5,4)), 
               legend, 
               widths=unit.c(unit(1, "npc") - legend$widths[2], legend$widths[2]),  
               ncol=2)
dev.off()

