# This script produces RIT Season-to-Season Arrow Graphs (or waht KIPP DC calls
# waterfall charts).  It requres the functions in the MAP_helpers.R scropt

library('ProjectTemplate')
load.project()


todays.date<-format(Sys.time(), "%y%m%d")


#KAPS and KAMS combined
pdf(file=paste0("graphs/Spring13_MAP_KAPS_KAMS_",todays.date,".pdf"), height=10.5, width=8)
#pdf(file="graphs/test.pdf", height=10.5, width=8)

for(s in sort(unique(map.scores.by.grade$Subject))){
  dfp<-map.scores.by.grade[Subject==s ,] #DataFrame to Plot
  for(g in as.character(sort(unique(dfp[,Fall12_Grade])))){
    ptitle <- paste0("KAPS/KAMS 2012-13 MAP Scores ",g," ",s, "\nFall and Spring RIT Scores vs Expected Growth and College Ready Growth\nby Fall Quartile")
    p<-plot_MAP_Results_and_Goals(dfp[Fall12_Grade==g,], ptitle, labxpos=113, minx=104,alp=.5)
    print(p)
  }
}
dev.off()

#KAMS only 
#pdf(file="graphs/Spring13_MAP_KAMS_130401.pdf", height=10.5, width=8)

#for(s in sort(unique(map.scores.kams$Subject))){
#  dfp<-map.scores.kams[Subject==s] #DataFrame to Plot
#  for(g in c(5:8)){
#    ptitle <- paste("KAMS 2013 Winter MAP Grade ",g," ",s,"\nFall and Winter RIT Scores vs Expected Growth, and College Ready Growth\nby Fall Quartile",sep="")
#    p<-plot_MAP_Results_and_Goals_2(dfp[Fall12_Grade==g],ptitle, labxpos=113, minx=104,alp=.3)
#    print(p)
#  }
#}
#dev.off()