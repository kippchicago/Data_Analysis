map.masked<-copy(map.scores.by.grade[(Fall12_Grade==5 | Fall12_Grade==6) & Subject!="General Science"])

map.masked<-data.table(ddply(.data=map.masked, .variables=c("Subject", "Fall12_Grade"), function(x) orderid(x,"Fall12_RIT")))
map.masked[,StudentFirstLastName:=as.character(ID)]

map.masked[,StudentFirstLastNameRIT:=paste(ID, Fall12_RIT)]


map.masked[GrowthCat=="Negative", StudentFirstLastNameRIT:=paste("X",StudentFirstLastNameRIT,sep=" ")]

map.masked[GrowthCat=="Negative", StudentFirstLastName:=paste("X",StudentFirstLastName,sep=" ")]

#Add 'O' in front of students who had negative growth


map.masked[GrowthCat=="Positive", StudentFirstLastNameRIT:=paste("O",StudentFirstLastNameRIT,sep=" ")]

map.masked[GrowthCat=="Positive", StudentFirstLastName:=paste("O",StudentFirstLastName,sep=" ")]


# KAMS and KCCP only 
pdf(file=paste0("graphs/Spring13_MAP_KAMS_KCCP_",todays.date,"_masked_by_id.pdf"), height=10.5, width=8)

for(s in sort(unique(map.masked[SchoolInitials!="KAPS", Subject]))){
    dfp<-map.masked[Subject==s] #DataFrame to Plot
    for(g in as.character(sort(unique(dfp[,Fall12_Grade])))){
      ptitle <- paste0("KIPP Chicago 2012-13 MAP Scores ",g," ",s, "\nFall and Spring RIT Scores vs Expected Growth and College Ready Growth\nby Fall Quartile")
      p<-plot_MAP_Results_and_Goals(dfp[Fall12_Grade==g,], ptitle, labxpos=100, minx=95,alp=.6)
      print(p)
  }  
}

dev.off()