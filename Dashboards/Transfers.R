
library(plyr)
library(ggplot2)
library(lubridate)
library(RJDBC) #To get data form PowerSchool DB
library(gridExtra)
library(xtable)

##Pull all students enrolled 
drvr <- JDBC("oracle.jdbc.driver.OracleDriver", "/Users/chaid/Dropbox/JDBC Drivers/ojdbc5.jar","")
pscon <- dbConnect(drvr,"jdbc:oracle:thin:@10.160.29.47:1521:IL039","psnavigator","laidephy")

#all students enrolled on October 3, 2012
Enrolled.121003<-dbGetQuery(pscon, "SELECT
m.SchoolID,
s.student_number as StudentID,
s.first_name,
s.middle_name,
s.last_name,
m.grade_level,
s.ethnicity AS Race_ID,
s.gender,
s.dob,
s.entrydate,
s.SCHOOLENTRYDATE,
s.DISTRICTENTRYDATE,
s.EXITDATE,
s.exitcode,
s.EXITCOMMENT
FROM PS_Membership_Defaults m
JOIN STUDENTS s
ON m.studentid = s.id
WHERE m.calendardate = '3-OCT-12'
ORDER BY schoolid, grade_level
")

#all Students enrolled on Oct 31 who have exit dates prior to 6/14/2013

Xfers.HSR<-subset(Enrolled.121003, ymd_hms(Enrolled.121003$EXITDATE)<ymd("13-06-14"))

#All students who transferred this year, as long as they were enrolled anytime during the year
Xfers.All<-dbGetQuery(pscon, "
SELECT
s.student_number as StudentID,
s.first_name,
s.middle_name,
s.last_name,
s.grade_level,
s.ethnicity AS Race_ID,
s.gender,
s.dob,
s.entrydate,
s.SCHOOLENTRYDATE,
s.DISTRICTENTRYDATE,
s.EXITDATE,
s.exitcode,
s.EXITCOMMENT
FROM STUDENTS s
WHERE s.exitdate > '14-AUG-12'
  AND s.exitdate < '14-JUN-13'
	AND s.exitcode!=99
ORDER BY schoolid, grade_level                    
")

#All students who transferred after 10/1/2012
Xfers.All.post121001<-dbGetQuery(pscon, "
SELECT
s.student_number as StudentID,
s.first_name,
s.middle_name,
s.last_name,
s.schoolid,
s.grade_level,
s.ethnicity AS Race_ID,
s.gender,
s.dob,
s.entrydate,
s.SCHOOLENTRYDATE,
s.DISTRICTENTRYDATE,
s.EXITDATE,
s.exitcode,
s.EXITCOMMENT
FROM STUDENTS s
WHERE s.exitdate > '1-OCT-12'
  AND s.exitdate < '14-JUN-13'
  AND s.exitcode!=99
ORDER BY schoolid, grade_level                    
")


#Go forrward for now with post 10/1/12 with anyone who's entered or exited.

#extract month and date of transfers
Xfers.HSR$Month<-month(ymd_hms(Xfers.HSR$EXITDATE), label=TRUE)
Xfers.HSR$Week<-week(ymd_hms(Xfers.HSR$EXITDATE))

xfer.total.monthly.by.school<-arrange(ddply(Xfers.HSR, .(SCHOOLID, Month), summarise, N=length(Month)), Month)

xfer.total.weekly.by.school<-arrange(ddply(Xfers.HSR, .(SCHOOLID, Week), summarise, N=length(Week)), Week)

xfer.total.cum.monthly<-ddply(xfer.total.monthly.by.school, .(SCHOOLID), transform, YTD=cumsum(N))

xfer.total.cum.weekly<-ddply(xfer.total.weekly.by.school, .(SCHOOLID), transform, YTD=cumsum(N))

xfer.total.cum.monthly$School[xfer.total.cum.monthly$SCHOOLID==78102]<-"KAPS"
xfer.total.cum.monthly$School[xfer.total.cum.monthly$SCHOOLID==7810]<-"KAMS"
xfer.total.cum.monthly$School[xfer.total.cum.monthly$SCHOOLID==400146]<-"KCCP"

####Need to build data fram that contains actual YTD, as well as 10% annual ceiling for each school.  Months need to 
# start with October
mons<-c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")
mons<-factor(mons, levels=mons, ordered=TRUE)

School<-c("KAPS", "KAMS", "KCCP")
School<-factor(School, levels=School)

xfer.total.cum.monthly$School<-factor(xfer.total.cum.monthly$School, level=School)



redline<-ddply(Enrolled.121003, .(SCHOOLID), summarise, N.Enrolled=length(STUDENTID))
redline$N.10pct<-redline$N.Enrolled*.1
redline$line.month<-redline$N.10pct/12
redline$School<-School
redline<-redline[,c("School", "line.month")]




xferplot.data<-expand.grid(Month=mons, School=School, Variable=c("Cumulative Transfers", "Ceiling"))
xferplot.data<-merge(x=xferplot.data, y=redline, by="School",all.x=TRUE )
names(xferplot.data)[4]<-"Value"
xferplot.data$Value[xferplot.data$Variable=="Cumulative Transfers"]<-NA

xfer.cums<-xfer.total.cum.monthly[,c("School", "Month", "N")]
xfer.cums$Variable<-"Cumulative Transfers"
xferplot.merge<-merge(xferplot.data, xfer.cums, by=c("School", "Month", "Variable"), all.x=TRUE)
xferplot.merge$Value[!is.na(xferplot.merge$N)]<-xferplot.merge$N[!is.na(xferplot.merge$N)]
xferplot.merge<-arrange(xferplot.merge[,c(1:4)], School, Variable, Month)

cum.na <- function(x) { 
  x[which(is.na(x))] <- 0 
  return(cumsum(x)) 
} 

xferplot<-arrange(ddply(xferplot.merge, .(School, Variable), transform, CumVal= cum.na(Value)),School, Variable, Month)

xferplot$Value[!is.na(xferplot$Value)]<-xferplot$CumVal[!is.na(xferplot$Value)]

#Plots 
g.plot<-ggplot(data=subset(xferplot, Variable=="Ceiling"), aes(x=Month, y=Value)) + geom_line(aes(group=Variable), color="red") + geom_bar(data=subset(xferplot, Variable!="Ceiling"), aes(x=Month, y=Value), fill="#439539", width=.5) +  facet_grid(School~., scale="free_y")

#table of Transfers by schoool 

Xfersreasons<-data.frame(EXITCODE=c(1:11,99),Reason=c("Dropped Out", "Moved", "Transport", "Expelled", "Behavior/Discipline", "Academics", "Avoid Retention", "Special Needs", "Other", "Don't Know", "Xfer Other KIPP", "DNA"))

Xfer.table<-merge(Xfers.HSR,Xfersreasons, all.x=TRUE)
Xfer.table<-cast( ddply(Xfer.table, .(Reason, SCHOOLID), function(df)c(N=nrow(df))), Reason~SCHOOLID, margins=TRUE, fun.aggregate=sum)
names(Xfer.table)<-c("Xfer Reasons", "KAMS", "KAPS", "KCCP", "Region")
levels(Xfer.table[,1])[13]<-"Total"
colnames(Xfer.table)
g.tbl<-tableGrob(xtable(Xfer.table))

#Combine plot
grid.newpage()
grid.arrange(g.plot, g.tbl, widths=c(2/3,1/3),ncol=2)












