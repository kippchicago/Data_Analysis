library(ProjectTemplate)
load.project()

map1213.create <- copy(map1213[SchoolName="KIPP Create Middle School"])

map1213.create[,list(TotMet=sum(Spring13_RIT-Fall12_RIT>=ReportedFallToSpringGrowth),PctMet =sum(Spring13_RIT-Fall12_RIT>=ReportedFallToSpringGrowth)/.N, AvgFall=mean(Fall12_RIT), AvgSpring=mean(Spring13_RIT), Tested=.N), by=list(Subject, Fall12_Grade)]