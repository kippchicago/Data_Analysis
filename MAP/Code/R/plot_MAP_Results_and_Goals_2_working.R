#New Goals and Achievement for MAP results for FAll12 to Winter13.

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
drvr <- JDBC("com.mysql.jdbc.Driver","/Users/chaid/Dropbox/JDBC Drivers/mysql-connector-java-5.0.8/mysql-connector-java-5.0.8-bin.jar","")

con <- dbConnect(drvr,"jdbc:mysql://54.245.118.235/db_kippchidata", "chaid", "haiKIPP1" )


#Need to modify the pull of data from MySQL Data Analysis DB server 
map.scores<-dbGetQuery(con, 'CALL GetMAPResultsFromTo("Fall12","Winter13");')

#Get grades properly ordered
map.scores$Fall12_Grade <- factor(map.scores$Fall12_Grade, levels=c("0", "1","2", "5", "6","7","8"))
levels(map.scores$Fall12_Grade) <- c("K", "1", "2", "5", "6","7","8")

map.scores$Spring13_Grade <- factor(map.scores$Spring13_Grade, levels=c("0", "1","2", "5", "6","7","8"))
levels(map.scores$Spring13_Grade) <- c("K", "1", "2", "5", "6","7","8")

#SEt Targets
#get z score (i.e., number of standard deviations) that corresponds to 75th percentile
sigma<-qnorm(.75)
#add simga*SD to mean and round to integer
map.scores$GrowthPctl75th<-round(map.scores$TypicalFallToWinterGrowth + sigma*map.scores$SDFallToWinterGrowth,0)

#calculate targets
map.scores$GrowthTargets<-map.scores$Fall12_RIT+map.scores$GrowthPctl75th




#Combine Student First and Last Names into one field

map.scores$StudentLastFirstName<-paste(map.scores$StudentLastName, map.scores$StudentFirstName, sep=", ")
map.scores$StudentFirstLastName<-paste(map.scores$StudentFirstName, map.scores$StudentLastName, sep=" ")

#source some helper functions
source("MAP_helper_functions.R")

#cut scores by grade
map.scores.by.grade<-ddply(map.scores, .(Subject, SchoolName,Fall12_Grade), function(df) orderid(df,df$Fall12_RIT))

#Relevel subject factors 
map.scores.by.grade$Subject<-factor(map.scores.by.grade$Subject, levels=c("Mathematics", "Reading", "Language Usage", "General Science"))

#make data.table for easier subsetting and assignment
map.scores.by.grade<-data.table(map.scores.by.grade)

#set growth Categories (note order matters here to get all the categories right)

map.scores.by.grade[Spring13_RIT<Fall12_RIT, GrowthCat:="Negative"]
map.scores.by.grade[Spring13_RIT>Fall12_RIT, GrowthCat:="Positive"]
map.scores.by.grade[Spring13_RIT>=Fall12_RIT+ReportedFallToWinterGrowth, GrowthCat:="Typical"]
map.scores.by.grade[Spring13_RIT>=Fall12_RIT+GrowthPctl75th, GrowthCat:="College Ready"]

#Add X in front of students who had negative growth

map.scores.by.grade[GrowthCat=="Negative", StudentFirstLastName:=paste("X",StudentFirstLastName,sep=" ")]


#map.scores.by.grade<-map.scores.by.grade[SchoolName!="KIPP Ascend Middle School"]

map.scores.kams <- map.scores.by.grade[SchoolName=="KIPP Ascend Middle School"]



p <- ggplot(subset(map.scores.by.grade, Grade==1 & Subject=="Reading"), aes(x=Fall12_RIT, y=OrderID)) +
  geom_segment(data=subset(map.scores.by.grade, Grade==1 & Subject=="Reading" & GrowthCat=="College Ready"), aes(x=Fall12_RIT, xend=Spring13_RIT, y=OrderID, yend=OrderID), arrow = arrow(length = unit(0.1,"cm")), color="#FEBC11") +
  geom_segment(data=subset(map.scores.by.grade, Grade==1 & Subject=="Reading" & GrowthCat=="Typical"), aes(x=Fall12_RIT, xend=Spring13_RIT, y=OrderID, yend=OrderID), , arrow = arrow(length = unit(0.1,"cm")), color="#CFCCC1") +
  geom_segment(data=subset(map.scores.by.grade, Grade==1 & Subject=="Reading" & GrowthCat=="Positive"), aes(x=Fall12_RIT, xend=Spring13_RIT, y=OrderID, yend=OrderID), , arrow = arrow(length = unit(0.1,"cm")), color="#C49A6C") +
  geom_segment(data=subset(map.scores.by.grade, Grade==1 & Subject=="Reading" & GrowthCat=="Negative"), aes(x=Fall12_RIT, xend=Spring13_RIT, y=OrderID, yend=OrderID), , arrow = arrow(length = unit(0.1,"cm")), color="red") +
  geom_text(aes(x=Spring13_RIT+.5, color=as.factor(Spring13_Quartile), label=Spring13_RIT), size=2, hjust=0) + 
  geom_text(aes(x=Fall12_RIT-1, color=as.factor(Quartile), label=StudentFirstLastName), size=2, hjust=1) + 
geom_point(aes(color=as.factor(Quartile)), size=pointsize) +
  geom_text(aes(x=Fall12_RIT+1, color=as.factor(Quartile), label=Fall12_RIT), size=2, hjust=0) +
  geom_point(aes(x=Fall12_RIT + ReportedFallToWinterGrowth, y=OrderID), color="#CFCCC1", size=pointsize, alpha=.4) +
  geom_text(aes(x=Fall12_RIT + ReportedFallToWinterGrowth+1, label=Fall12_RIT + ReportedFallToWinterGrowth), color="#CFCCC1", size=2, hjust=0, alpha=.4) +
  geom_point(aes(x=GrowthTargets, y=OrderID), color="#FEBC11", size=pointsize, alpha=.4) + 
  geom_text(aes(x=GrowthTargets+1, label=GrowthTargets), color="#FEBC11", size=2, hjust=0, alpha=.4) +
  facet_grid(Quartile~., scale="free_y", space = "free_y", as.table=FALSE) +
  scale_colour_discrete(kippcols) + 
  scale_y_continuous(" ", breaks=map.scores.by.grade$OrderID, expand=c(0,1)) + 
  theme(axis.text.y = element_text(size=3, hjust=1)) + 
  theme(legend.position = "none") + 
  scale_x_continuous("RIT Score") + 
  expand_limits(x=115)+
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA), # or element_blank()
    # panel.grid.minor = element_blank(), 
    # panel.grid.major = element_blank(),
    plot.background = element_rect(fill = "transparent",colour = NA),
    axis.text.x = element_text(size=15),
    axis.text.y = element_blank(), 
    #axis.title.y = element_blank(), 
    axis.ticks=element_blank(),
    
    strip.text.x=element_text(size=15),
    strip.text.y=element_text(size=15,angle=0), 
    strip.background=element_rect(fill="#F4EFEB", colour=NA),
    plot.title=element_text(size=12)
  ) +
  ggtitle("2012 Fall 1st Grade Reading\nRIT Scores, 
                Expected Growth, and College Ready Growth\nby Quartile")  

###############
##############
###############

#Function to report MAP RIT scores and goals (expected adn expected at 7th percentile) with summary stats
plot_MAP_Results_and_Goals_2 <- function (df, plottitle=" ",labxpos=115, minx=105,alp=1) {
  kippcols<-c("#E27425", "#FEBC11", "#255694", "A7CFEE")
  
  #Plot points for Fall RIT Score, Expected Growth, College Ready Growth, ordered by Fall RIT, Names on Y axis
  pointsize<-2
  p <- ggplot(df, aes(x=Fall12_RIT, y=OrderID)) +
    
      geom_segment(data=subset(df, GrowthCat=="College Ready"), aes(x=Fall12_RIT, xend=Spring13_RIT, y=OrderID, yend=OrderID), arrow = arrow(length = unit(0.1,"cm")), color="#FEBC11") +
      geom_segment(data=subset(df, GrowthCat=="Typical"), aes(x=Fall12_RIT, xend=Spring13_RIT, y=OrderID, yend=OrderID), arrow = arrow(length = unit(0.1,"cm")), color="#CFCCC1") +
      geom_segment(data=subset(df, GrowthCat=="Positive"), aes(x=Fall12_RIT, xend=Spring13_RIT, y=OrderID, yend=OrderID), arrow = arrow(length = unit(0.1,"cm")), color="#C49A6C") +
      geom_segment(data=subset(df, GrowthCat=="Negative"), aes(x=Fall12_RIT, xend=Spring13_RIT, y=OrderID, yend=OrderID),  arrow = arrow(length = unit(0.1,"cm")), color="red") +
      geom_text(data=df[GrowthCat!="Negative"],aes(x=Spring13_RIT+.5, color=as.factor(Spring13_Quartile), label=Spring13_RIT), size=2, hjust=0) + 
     geom_text(data=df[GrowthCat=="Negative"],aes(x=Spring13_RIT-.5, color=as.factor(Spring13_Quartile), label=Spring13_RIT), size=2, hjust=1) + 
    geom_text(data=df[GrowthCat!="Negative"],aes(x=Fall12_RIT-1, color=as.factor(Fall12_Quartile), label=StudentFirstLastName), size=2, hjust=1) +
    geom_text(data=df[GrowthCat=="Negative"],aes(x=Spring13_RIT-4, label=StudentFirstLastName), color="red" ,size=2, hjust=1) +
    geom_point(aes(color=as.factor(Fall12_Quartile)), size=pointsize) +
    geom_text(aes(x=Fall12_RIT+1, color=as.factor(Fall12_Quartile), label=Fall12_RIT), size=2, hjust=0) +
    geom_point(aes(x=Fall12_RIT + TypicalFallToWinterGrowth, y=OrderID), color="#CFCCC1", size=pointsize, alpha=alp) +
    geom_text(aes(x=Fall12_RIT + TypicalFallToWinterGrowth+1, label=round(Fall12_RIT + TypicalFallToWinterGrowth)), color="#CFCCC1", size=2, hjust=0, alpha=alp) +
    geom_point(aes(x=GrowthTargets, y=OrderID), color="#FEBC11", size=pointsize, alpha=alp) + 
    geom_text(aes(x=GrowthTargets+1, label=GrowthTargets), color="#FEBC11", size=2, hjust=0, alpha=alp) +
    facet_grid(Fall12_Quartile~., scale="free_y", space = "free_y", as.table=FALSE) +
    scale_colour_discrete(kippcols) + 
    scale_y_continuous(" ", breaks=df$OrderID, expand=c(0,1.5)) + 
    theme(axis.text.y = element_text(size=3, hjust=1)) + 
    theme(legend.position = "none") + 
    scale_x_continuous("RIT Score") + 
    expand_limits(x=minx)+
    theme(
      panel.background = element_rect(fill = "transparent",colour = NA), # or element_blank()
      # panel.grid.minor = element_blank(), 
      # panel.grid.major = element_blank(),
      plot.background = element_rect(fill = "transparent",colour = NA),
      axis.text.x = element_text(size=15),
      axis.text.y = element_blank(), 
      #axis.title.y = element_blank(), 
      axis.ticks=element_blank(),
      
      strip.text.x=element_text(size=15),
      strip.text.y=element_text(size=15,angle=0), 
      strip.background=element_rect(fill="#F4EFEB", colour=NA),
      plot.title=element_text(size=12)
    ) +
    ggtitle(plottitle)
  
  
  ###Let's add some summary labels by quaritle to p
  
  #First get the per panel data I want count by quartile, avg y-position (given by OrderID) by quartile,
  #  avg RIT by quartile, and percent of quartile students to total studens.
  
  qrtl.labels<-get_group_stats(as.data.frame(df), grp="Fall12_Quartile")
  wqrtl.labels<-get_group_stats(as.data.frame(df), grp="Spring13_Quartile")
  #add a column with the actual label text
  qrtl.labels$CountLabel<-paste(qrtl.labels$CountStudents," students (",round(qrtl.labels$PctofTotal*100),"%)", sep="")
  wqrtl.labels$CountLabel<-paste(wqrtl.labels$CountStudents," students (",round(wqrtl.labels$PctofTotal*100),"%)", sep="")
  
  qrtl.labels$AvgLabel<-paste("F Avg RIT = ",round(qrtl.labels$AvgQrtlRIT))
  wqrtl.labels$AvgLabel<-paste("W Avg RIT = ",round(wqrtl.labels$AvgQrtlRIT))
  
  names(wqrtl.labels)[1]<-"Fall12_Quartile" # realigning to the proper facet
  wqrtl.labels$AvgCountID<-qrtl.labels$AvgCountID-5
  #eyeballed X position
  qrtl.labels$xpos<-rep(labxpos,nrow(qrtl.labels))
  wqrtl.labels$xpos<-rep(labxpos,nrow(wqrtl.labels))
  
  #now adding this info to the plot p
  p <- p + geom_text(data=qrtl.labels, aes(x=xpos, y=AvgCountID, color=factor(Fall12_Quartile),label=CountLabel),vjust=0, size=3.25) +
    geom_text(data=qrtl.labels, aes(x=xpos, y=AvgCountID, color=factor(Fall12_Quartile),label=AvgLabel),vjust=1.5, size=3.25) 

  p <- p + geom_text(data=wqrtl.labels, aes(x=xpos, y=AvgCountID, color=factor(Fall12_Quartile),label=CountLabel),vjust=0, size=3.25) +
    geom_text(data=wqrtl.labels, aes(x=xpos, y=AvgCountID, color=factor(Fall12_Quartile),label=AvgLabel),vjust=1.5, size=3.25) 
  
  
  
  p
}


#KAPS and KCCP combined
pdf(file="../../Figures/Spring13_MAP_KAPS_KCCP_13021.pdf", height=10.5, width=8)

for(s in sort(unique(map.scores$Subject))){
  dfp<-map.scores.by.grade[Subject==s] #DataFrame to Plot
  for(g in as.character(sort(unique(dfp$Grade)))){
    ptitle <- paste("KAPS/KCCP 2012 Winter MAP Grade ",g," ",s,"\nFall and Winter RIT Scores vs Expected Growth, and College Ready Growth\nby FallQuartile",sep="")
    p<-plot_MAP_Results_and_Goals_2(subset(dfp,Grade==g),ptitle, labxpos=113, minx=104,alp=.3)
    print(p)
  }
}
dev.off()

#KAMS only 
pdf(file="../../Figures/Spring13_MAP_KAMS_130401.pdf", height=10.5, width=8)

for(s in sort(unique(map.scores.kams$Subject))){
  dfp<-map.scores.kams[Subject==s] #DataFrame to Plot
  for(g in c(5:8)){
    ptitle <- paste("KAMS 2013 Winter MAP Grade ",g," ",s,"\nFall and Winter RIT Scores vs Expected Growth, and College Ready Growth\nby Fall Quartile",sep="")
    p<-plot_MAP_Results_and_Goals_2(dfp[Fall12_Grade==g],ptitle, labxpos=113, minx=104,alp=.3)
    print(p)
  }
}
dev.off()
  