#load libraries
library(reshape)
library(ggplot2)
library(grid)
library(RJDBC)
library(plyr)
library(data.table)

#set working directory
setwd("~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Code/R")

#open DB connectiond
drvr <- JDBC("com.mysql.jdbc.Driver","/Users/chaid/Dropbox/JDBC Drivers/mysql-connector-java-5.0.8/mysql-connector-java-5.0.8-bin.jar",""
con <- dbConnect(drvr,"jdbc:mysql://54.245.118.235/db_kippchidata", "chaid", "haiKIPP1" )

map.scores<-dbGetQuery(con, 'CALL GetMAPResultsFromTo("Fall12","Winter13");')
map.scores<-data.table(map.scores)

# Fall 12
map.scores[, AboveGradeLevel:=0]
map.scores[Fall12_Pctl>=50, AboveGradeLevel:=1]

map.scores.8 <- map.scores[Fall12_Grade==8]


map.scores.8[,labely:= seq(0, 100, by=(100-0)/(.N-1)),by=Subject]

map.scores.8[,labelx:= min(Fall12_RIT),by=Subject]
             
             
             
map.scores.8[,list(PctAboveGradeLevel=mean(AboveGradeLevel)),by=Subject]

map.GL.summary.8[,Label:=paste("> Grade Level = \n ",round(PctAboveGL*100),"% (",NAboveGGL,"/",N,")",sep="")]
             
map.GL.summary.8[,PctTarget:=c(.73,.65)]
             
map.GL.summary.8[,N2Move:=round(N*PctTarget-NAboveGGL)]

map.GL.summary.8[,Label2Move:=paste("# Needed for \n",round(PctTarget*100),"% Target = ",N2Move,sep="")]  
             

             
map.GL.summary.8[,hl.start:=map.scores.8[map.GL.summary.8[,N-NAboveGGL],labely]]
map.GL.summary.8[,hl.end:=map.scores.8[map.GL.summary.8[,N-NAboveGGL-N2Move],labely]]

map.GL.summary.8[,hl.x:=map.scores.8[,min(Fall12_RIT)-63,by=Subject][,V1]]
             
             

kams.GL.plot<-ggplot(map.scores.8,aes(x=Fall12_RIT, y=Fall12_Pctl)) + 
  geom_hline(aes(yintercept=50), alpha=.4) +
  geom_text(aes(color=as.factor(AboveGradeLevel), x=labelx-10, y=labely, label=paste(StudentFirstName, StudentLastName," (",Fall12_RIT,"/",Fall12_Pctl,")")),hjust=1, size=2.5, show_guide=FALSE) + 
  geom_point(aes(color=as.factor(AboveGradeLevel), shape=as.factor(Fall12_Quartile)), alpha=.3, size=4,show_guide=FALSE) + 
  geom_segment(aes(x=labelx-9,xend=Fall12_RIT-1,y=labely,yend=Fall12_Pctl, color=as.factor(AboveGradeLevel)), alpha=.2,show_guide=FALSE) + 
  geom_text(data=map.GL.summary.8,aes(x=240, y=16, label=Label), size=4) +
  geom_text(data=map.GL.summary.8,aes(x=240, y=10, label=Label2Move), size=4, color="#439539") +
  geom_segment(data=map.GL.summary.8,aes(x=hl.x,xend=hl.x, y=hl.start,yend=hl.end), color="#439539",size=2,lineend = "butt") +
  facet_grid(.~Subject) +
  xlim(c(100,255)) +   
  scale_shape_manual(values=c(15:18)) +  
  scale_color_manual(values=c("#E27425","#60A2D7")) +
  theme_bw()+
  ggtitle("MAP KAMS: 8th Grade Fall 2012 RIT vs National Percentile by Student")             
        
kams.GL.plot  
             
ggsave("../../Figures/MAP_KAMS_Intervention.pdf",kams.GL.plot,width=10.75, height=8.25, units="in")
    

             
             
             
# Winter 13, 8th
map.scores[, AboveGradeLevel:=0]
map.scores[Fall_Pctl>=50, AboveGradeLevel:=1]
             
map.scores.8 <- map.scores[Fall_Grade==8]

setkey(map.scores.8, Subject,Fall_Pctl)
             
             
map.scores.8[,labely:= seq(0, 100, by=(100-0)/(.N-1)),by=Subject]
             
map.scores.8[,labelx:= min(Fall_RIT),by=Subject]
             
             
             
map.scores.8[,list(PctAboveGL=mean(AboveGradeLevel)),by=Subject]

# create student name (RIT / Pctl) label
map.scores.8[,student.label:=paste(StudentFirstName, StudentLastName,
                                   " (",Fall_RIT,"/",Fall_Pctl,")")]             
             
map.GL.summary.8<-map.scores.8[,list(.N, NAboveGGL = sum(AboveGradeLevel),PctAboveGL=mean(AboveGradeLevel)),by=Subject]

             
map.GL.summary.8[,Label:=paste("> Grade Level = \n ",round(PctAboveGL*100),"% (",NAboveGGL,"/",N,")",sep="")]
             
map.GL.summary.8[,PctTarget:=c(.73,.65)]
             
map.GL.summary.8[,N2Move:=round(N*PctTarget-NAboveGGL)]
             
map.GL.summary.8[,Label2Move:=paste("# Needed for \n",round(PctTarget*100),"% Target = ",N2Move,sep="")]  
             
             
             
map.GL.summary.8[,hl.start:=map.scores.8[map.GL.summary.8[,N-NAboveGGL],labely]]
map.GL.summary.8[,hl.end:=map.scores.8[map.GL.summary.8[,N-NAboveGGL-N2Move],labely]]
             
map.GL.summary.8[,hl.x:=map.scores.8[,min(labelx-67),by=Subject][,V1]]
          

             
kams.GL.plot<-ggplot(map.scores.8,aes(x=Fall_RIT, y=Fall_Pctl)) + 
               geom_hline(aes(yintercept=50), alpha=.4) +
               geom_text(aes(color=as.factor(AboveGradeLevel), x=labelx-10, 
                             y=labely, label=student.label),hjust=1, size=2.5, show_guide=FALSE) + 
               geom_point(aes(color=as.factor(AboveGradeLevel), shape=as.factor(Fall_Quartile)), alpha=.3, size=4,show_guide=FALSE) + 
               geom_segment(aes(x=labelx-9,xend=Fall_RIT-1,y=labely,yend=Fall_Pctl, color=as.factor(AboveGradeLevel)), alpha=.2,show_guide=FALSE) + 
               geom_text(data=map.GL.summary.8,aes(x=240, y=16, label=Label), size=4) +
               geom_text(data=map.GL.summary.8,aes(x=240, y=10, label=Label2Move), size=4, color="#439539") +
               geom_segment(data=map.GL.summary.8,aes(x=hl.x,xend=hl.x, y=hl.start,yend=hl.end), color="#439539",size=2,lineend = "butt") +
               facet_grid(.~Subject) +
               xlim(c(90,255)) +   
               scale_shape_manual(values=c(15:18)) +  
               scale_color_manual(values=c("#E27425","#60A2D7")) +
               theme_bw()+
               ggtitle("MAP KAMS: 8th Grade Winter 2013 RIT vs National Percentile by Student")             
             
kams.GL.plot  
             
ggsave("../../Figures/MAP_KAMS_W13_Intervention.pdf",kams.GL.plot,width=10.75, height=8.25, units="in")
             
             
##############################
# Winter 13, 7th
             
map.scores.6 <- map.scores[Fall_Grade==7]
             
             setkey(map.scores.6, Subject,Fall_Pctl)
             
             
             map.scores.6[,labely:= seq(0, 100, by=(100-0)/(.N-1)),by=Subject]
             
             map.scores.6[,labelx:= min(Fall_RIT),by=Subject]
             
             
             
             map.scores.6[,list(PctAboveGL=mean(AboveGradeLevel)),by=Subject]
             
             # create student name (RIT / Pctl) label
             map.scores.6[,student.label:=paste(StudentFirstName, StudentLastName,
                                                " (",Fall_RIT,"/",Fall_Pctl,")")]             
             
             map.GL.summary.6<-map.scores.6[,list(.N, NAboveGGL = sum(AboveGradeLevel),PctAboveGL=mean(AboveGradeLevel)),by=Subject]
             
             
             map.GL.summary.6[,Label:=paste("> Grade Level = \n ",round(PctAboveGL*100),"% (",NAboveGGL,"/",N,")",sep="")]
             
             map.GL.summary.6[,PctTarget:=c(.73,.65)]
             
             map.GL.summary.6[,N2Move:=round(N*PctTarget-NAboveGGL)]
             
             map.GL.summary.6[,Label2Move:=paste("# Needed for \n",round(PctTarget*100),"% Target = ",N2Move,sep="")]  
             
             
             
             map.GL.summary.6[,hl.start:=map.scores.6[map.GL.summary.6[,N-NAboveGGL],labely]]
             map.GL.summary.6[,hl.end:=map.scores.6[map.GL.summary.6[,N-NAboveGGL-N2Move],labely]]
             
             map.GL.summary.6[,hl.x:=map.scores.6[,min(labelx-69),by=Subject][,V1]]
             
             
             
kams.GL.plot<-ggplot(map.scores.6,aes(x=Fall_RIT, y=Fall_Pctl)) + 
               geom_hline(aes(yintercept=50), alpha=.4) +
               geom_text(aes(color=as.factor(AboveGradeLevel), x=labelx-10, 
                             y=labely, label=student.label),hjust=1, size=2.5, show_guide=FALSE) + 
               geom_point(aes(color=as.factor(AboveGradeLevel), shape=as.factor(Fall_Quartile)), alpha=.3, size=4,show_guide=FALSE) + 
               geom_segment(aes(x=labelx-9,xend=Fall_RIT-1,y=labely,yend=Fall_Pctl, color=as.factor(AboveGradeLevel)), alpha=.2,show_guide=FALSE) + 
               geom_text(data=map.GL.summary.6,aes(x=240, y=16, label=Label), size=4) +
               geom_text(data=map.GL.summary.6,aes(x=240, y=10, label=Label2Move), size=4, color="#439539") +
               geom_segment(data=map.GL.summary.6,aes(x=hl.x,xend=hl.x, y=hl.start,yend=hl.end), color="#439539",size=2,lineend = "butt") +
               facet_grid(.~Subject) +
               xlim(c(80,255)) +   
               scale_shape_manual(values=c(15:18)) +  
               scale_color_manual(values=c("#E27425","#60A2D7")) +
               theme_bw()+
               ggtitle("MAP KAMS: 7th Grade Winter 2013 RIT vs National Percentile by Student")             
             
             kams.GL.plot  
             
             ggsave("../../Figures/MAP_KAMS_W13_7th_Intervention.pdf",kams.GL.plot,width=10.75, height=8.25, units="in")
             
##############################
# Fall 12, 6th
             
             map.scores[, AboveGradeLevel:=0]
             map.scores[Fall_Pctl>=50, AboveGradeLevel:=1]
             map.scores.6 <- map.scores[Fall_Grade==6]
             
             setkey(map.scores.6, Subject,Fall_Pctl)
             
             
             map.scores.6[,labely:= seq(0, 100, by=(100-0)/(.N-1)),by=Subject]
             
             map.scores.6[,labelx:= min(Fall_RIT),by=Subject]
             
             
             
             map.scores.6[,list(PctAboveGL=mean(AboveGradeLevel)),by=Subject]
             
             # create student name (RIT / Pctl) label
             map.scores.6[,student.label:=paste(StudentFirstName, StudentLastName,
                                                " (",Fall_RIT,"/",Fall_Pctl,")")]             
             
             map.GL.summary.6<-map.scores.6[,list(.N, NAboveGGL = sum(AboveGradeLevel),PctAboveGL=mean(AboveGradeLevel)),by=Subject]
             
             
             map.GL.summary.6[,Label:=paste("> Grade Level = \n ",round(PctAboveGL*100),"% (",NAboveGGL,"/",N,")",sep="")]
             
             map.GL.summary.6[,PctTarget:=c(.73,.65)]
             
             map.GL.summary.6[,N2Move:=round(N*PctTarget-NAboveGGL)]
             
             map.GL.summary.6[,Label2Move:=paste("# Needed for \n",round(PctTarget*100),"% Target = ",N2Move,sep="")]  
             
             
             
             map.GL.summary.6[,hl.start:=map.scores.6[map.GL.summary.6[,N-NAboveGGL],labely]]
             map.GL.summary.6[,hl.end:=map.scores.6[map.GL.summary.6[,N-NAboveGGL-N2Move],labely]]
             
             map.GL.summary.6[,hl.x:=map.scores.6[,min(labelx-69),by=Subject][,V1]]
             
             
             
             kams.GL.plot<-ggplot(map.scores.6,aes(x=Fall_RIT, y=Fall_Pctl)) + 
               geom_hline(aes(yintercept=50), alpha=.4) +
               geom_text(aes(color=as.factor(AboveGradeLevel), x=labelx-10, 
                             y=labely, label=student.label),hjust=1, size=2.5, show_guide=FALSE) + 
               geom_point(aes(color=as.factor(AboveGradeLevel), shape=as.factor(Fall_Quartile)), alpha=.3, size=4,show_guide=FALSE) + 
               geom_segment(aes(x=labelx-9,xend=Fall_RIT-1,y=labely,yend=Fall_Pctl, color=as.factor(AboveGradeLevel)), alpha=.2,show_guide=FALSE) + 
               geom_text(data=map.GL.summary.6,aes(x=240, y=16, label=Label), size=4) +
               geom_text(data=map.GL.summary.6,aes(x=240, y=10, label=Label2Move), size=4, color="#439539") +
               geom_segment(data=map.GL.summary.6,aes(x=hl.x,xend=hl.x, y=hl.start,yend=hl.end), color="#439539",size=2,lineend = "butt") +
               facet_grid(.~Subject) +
               xlim(c(80,255)) +   
               scale_shape_manual(values=c(15:18)) +  
               scale_color_manual(values=c("#E27425","#60A2D7")) +
               theme_bw()+
               ggtitle("MAP KAMS: 6th Grade FALL 2013 RIT vs National Percentile by Student")             
             
             kams.GL.plot  
             
             ggsave("../../Figures/MAP_KAMS_W13_6th_Intervention.pdf",kams.GL.plot,width=10.75, height=8.25, units="in")
             
             
             