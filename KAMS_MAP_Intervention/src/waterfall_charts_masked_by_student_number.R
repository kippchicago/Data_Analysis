# This script produces RIT Season-to-Season Arrow Graphs (or waht KIPP DC calls
# waterfall charts).  It requres the functions in the MAP_helpers.R scropt

library('ProjectTemplate')
load.project()


todays.date<-format(Sys.time(), "%y%m%d")


#All Schools
pdf(file=paste0("graphs/Spring13_MAP_KIPPChi_",todays.date,".pdf"), height=10.5, width=8)
#pdf(file="graphs/test.pdf", height=10.5, width=8)



for(s in sort(unique(map.scores.by.grade$Subject))){
  for(k in sort(unique(map.scores.by.grade[,SchoolInitials]))){
    dfp<-map.scores.by.grade[Subject==s & SchoolInitials==k ,] #DataFrame to Plot
    for(g in as.character(sort(unique(dfp[,Fall12_Grade])))){
      ptitle <- paste0(k, " 2012-13 MAP Scores ",g," ",s, "\nFall and Spring RIT Scores vs Expected Growth and College Ready Growth\nby Fall Quartile")
      p<-plot_MAP_Results_and_Goals(dfp[Fall12_Grade==g,], ptitle, labxpos=100, minx=95,alp=.6)
      print(p)
    }
  }  
}

dev.off()

# KAMS only 
pdf(file=paste0("graphs/Spring13_MAP_KAMS_",todays.date,".pdf"), height=10.5, width=8)

for(s in sort(unique(map.scores.by.grade[SchoolInitials=="KAMS", Subject]))){
  for(k in unique(map.scores.by.grade[SchoolInitials=="KAMS",SchoolInitials])){
    dfp<-map.scores.by.grade[Subject==s & SchoolInitials==k ,] #DataFrame to Plot
    for(g in as.character(sort(unique(dfp[,Fall12_Grade])))){
      ptitle <- paste0(k, " 2012-13 MAP Scores ",g," ",s, "\nFall and Spring RIT Scores vs Expected Growth and College Ready Growth\nby Fall Quartile")
      p<-plot_MAP_Results_and_Goals(dfp[Fall12_Grade==g,], ptitle, labxpos=100, minx=95,alp=.6)
      print(p)
    }
  }  
}

dev.off()

# KAPS only 
pdf(file=paste0("graphs/Spring13_MAP_KAPS_",todays.date,".pdf"), height=10.5, width=8)

for(s in sort(unique(map.scores.by.grade[SchoolInitials=="KAPS", Subject]))){
  for(k in unique(map.scores.by.grade[SchoolInitials=="KAPS",SchoolInitials])){
    dfp<-map.scores.by.grade[Subject==s & SchoolInitials==k ,] #DataFrame to Plot
    for(g in as.character(sort(unique(dfp[,Fall12_Grade])))){
      ptitle <- paste0(k, " 2012-13 MAP Scores ",g," ",s, "\nFall and Spring RIT Scores vs Expected Growth and College Ready Growth\nby Fall Quartile")
      p<-plot_MAP_Results_and_Goals(dfp[Fall12_Grade==g,], ptitle, labxpos=100, minx=95,alp=.6)
      print(p)
    }
  }  
}

dev.off()

# KCCP only 
pdf(file=paste0("graphs/Spring13_MAP_KCCP_",todays.date,".pdf"), height=10.5, width=8)

for(s in sort(unique(map.scores.by.grade[SchoolInitials=="KCCP", Subject]))){
  for(k in unique(map.scores.by.grade[SchoolInitials=="KCCP",SchoolInitials])){
    dfp<-map.scores.by.grade[Subject==s & SchoolInitials==k ,] #DataFrame to Plot
    for(g in as.character(sort(unique(dfp[,Fall12_Grade])))){
      ptitle <- paste0(k, " 2012-13 MAP Scores ",g," ",s, "\nFall and Spring RIT Scores vs Expected Growth and College Ready Growth\nby Fall Quartile")
      p<-plot_MAP_Results_and_Goals(dfp[Fall12_Grade==g,], ptitle, labxpos=100, minx=95,alp=.6)
      print(p)
    }
  }  
}

dev.off()
