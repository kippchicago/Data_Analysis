library(RJDBC)
library(plyr)
library(data.table)

drvr <- JDBC("com.mysql.jdbc.Driver","/Users/chaid/Dropbox/JDBC Drivers/mysql-connector-java-5.0.8/mysql-connector-java-5.0.8-bin.jar","")

con <- dbConnect(drvr,"jdbc:mysql://54.245.118.235/db_kippchidata", "chaid", "haiKIPP1" )

map.1213<-data.table(
            dbGetQuery(con, 
                       'CALL GetMAPResultsFromToByName("Fall12","Spring13");'))

map.1213



#map.1213[ClassName=="ND" & Subject=="Reading" & Diff %in% c(-1:-3)]
map.1213[,Target:=Fall12_RIT+ReportedFallToSpringGrowth]
map.1213[Spring13_RIT>=Target,Meets:=1]
map.1213[Spring13_RIT<Target,Meets:=0]
map.1213[,Diff:=Spring13_RIT-Target]

map.1213[ClassName=="ND" & Subject=="Reading" & Diff %in% c(-1:-3)]


map.1213[,c(pct.meets=sum(Meets)/.N, tot.meets=sum(Meets), tot.students=.N),by=list(Subject, Fall12_Grade)]
map.1213[,list(pct.meets=sum(Meets)/.N, tot.meets=sum(Meets), tot.students=.N),by=list(Subject, Fall12_Grade)]
map.pct.meets.1213<-map.1213[,list(pct.meets=sum(Meets)/.N, tot.meets=sum(Meets), tot.students=.N),by=list(Subject, Fall12_Grade)]
map.pct.meets.1213
setkey(map.pct.meets.1213, Subject, Fall12_Grade)
map.pct.meets.1213
map.1213[Subject=="Reading" & Fall12_Grade==5,NationalMean:=212.3]
map.1213[Subject=="Reading" & Fall12_Grade==6,NationalMean:=216.4]
map.1213[Subject=="Reading" & Fall12_Grade==7,NationalMean:=219.7]
map.1213[Subject=="Reading" & Fall12_Grade==8,NationalMean:=222.4]
map.1213[Subject=="Mathematics" & Fall12_Grade==5,NationalMean:=221]
map.1213[Subject=="Mathematics" & Fall12_Grade==6,NationalMean:=225.6]
map.1213[Subject=="Mathematics" & Fall12_Grade==7,NationalMean:=230.5]
map.1213[Subject=="Mathematics" & Fall12_Grade==8,NationalMean:=234.5]
map.1213[Spring13_RIT>=NationalMean, MeetsMean:=1]
map.1213[Spring13_RIT<NationalMean, MeetsMean:=0]


map.pct.meets.1213<-map.1213[,list(pct.meets=sum(Meets)/.N, tot.meets=sum(Meets), pct.mean=sum(MeetsMean)/.N, tot.mean=sum(MeetsMean),tot.students=.N),by=list(Subject, Fall12_Grade)]
map.summary.1213<-map.1213[,list(pct.meets=sum(Meets)/.N, tot.meets=sum(Meets), pct.mean=sum(MeetsMean)/.N, tot.mean=sum(MeetsMean),tot.students=.N),by=list(Subject, Fall12_Grade)]
map.summary.1213
setkey(map.summary.1213, Subject, Fall12_Grade)
map.summary.1213


map.1213[Spring13_Pctl>=50, Above50th:=1]
map.1213[Spring13_Pctl<50, Above50th:=0]

map.1213[Fall12_Pctl>=50, Fall12_Above50th:=1]
map.1213[Fall12_Pctl<50, Fall12_Above50th:=0]


map.summary.1213<-map.1213[,list(pct.meets=sum(Meets)/.N, tot.meets=sum(Meets),  pct.50th.fall=sum(Fall12_Above50th)/.N, tot.50th.fall=sum(Fall12_Above50th), pct.50th.spring=sum(Above50th)/.N, tot.50th.spring=sum(Above50th), tot.students=.N),by=list(Subject, Fall12_Grade)]

setkey(map.summary.1213, Subject, Fall12_Grade)

map.summary.1213

write.csv(map.summary.1213, 'reports/KAPMS_MAP_S13_summary_130530.csv')
write.csv(map.1213, file = "/reports/KAMS_MAP_S13_scores_130529.csv")