#extract month and date of transfers
#need cumulative summation that handles NAs as 0s for next function to work
cum.na <- function(x) { 
  x[which(is.na(x))] <- 0 
  return(cumsum(x)) 
} 
transferplot.data.prep <- function (transfers.data, enrolled.data) {
  require(lubridate)
  require(reshape2)
  
  transfers.data$Month<-lubridate::month(ymd_hms(transfers.data$EXITDATE), label=TRUE)
  transfers.data$Week<-lubridate::week(ymd_hms(transfers.data$EXITDATE))
  
  monthly.by.school<-arrange(ddply(transfers.data, .(SCHOOLID, Month), summarise, N=length(Month)), Month)
  
  cum.monthly<-ddply(monthly.by.school, .(SCHOOLID), transform, YTD=cumsum(N))
  
  cum.monthly$School[cum.monthly$SCHOOLID==78102]<-"KAPS"
  cum.monthly$School[cum.monthly$SCHOOLID==7810]<-"KAMS"
  cum.monthly$School[cum.monthly$SCHOOLID==400146]<-"KCCP"
  
  ####Need to build data fram that contains actual YTD, as well as 10% annual ceiling for each school.  Months need to 
  # start with October
  mons<-c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")
  mons<-factor(mons, levels=mons, ordered=TRUE)
  
  #School<-c("KAPS", "KAMS", "KCCP")
  School<-unique(cum.monthly$School)
  School<-factor(School, levels=School)
  
  cum.monthly$School<-factor(cum.monthly$School, level=School)
  
  
  
  redline<-ddply(enrolled.data, .(SCHOOLID), summarise, N.Enrolled=length(STUDENTID))
  redline$N.10pct<-redline$N.Enrolled*.1
  redline$line.month<-redline$N.10pct/12
  redline$School<-School
  redline<-redline[,c("School", "line.month")]
  
  
  
  
  xferplot.data<-expand.grid(Month=mons, School=School, Variable=c("Cumulative Transfers", "Ceiling"))
  xferplot.data<-merge(x=xferplot.data, y=redline, by="School",all.x=TRUE )
  names(xferplot.data)[4]<-"Value"
  xferplot.data$Value[xferplot.data$Variable=="Cumulative Transfers"]<-NA
  
  xfer.cums<-cum.monthly[,c("School", "Month", "N")]
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
  
  xferplot
}