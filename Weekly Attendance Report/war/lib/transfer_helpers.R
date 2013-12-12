rm#extract month and date of transfers
#need cumulative summation that handles NAs as 0s for next function to work

cum.na <- function(x) { 
  # Calcualtes cumsum by treating NAs as 0s
  # Args:
  #       x: a vector over which to calculate a cumulative sum
  # Returns:
  #       x: a vector fo the cumulative sums of the argument
  x[which(is.na(x))] <- 0 
  return(cumsum(x)) 
} 

transferplot.data.prep <- function (transfers.data, enrolled.data) {
  # Creates a ready to ploy month cumulative sum of powerschool exit data using the list fo transferd students 
  
  
  require(lubridate)
  require(reshape2)
  
  transfers.data$Month<-lubridate::month(ymd_hms(transfers.data$EXITDATE), label=TRUE)
  transfers.data$Week<-lubridate::week(ymd_hms(transfers.data$EXITDATE))
  
  monthly.by.school<-transfers.data[,list(N=.N), by=list(SCHOOLID, Month)][order(Month)]
  #monthly.by.school<-arrange(ddply(transfers.data, .(SCHOOLID, Month), summarise, N=length(Month)), Month)
  
  cum.monthly<-monthly.by.school[,list(Month, N, YTD=cumsum(N)), by=SCHOOLID]
  #cum.monthly<-ddply(monthly.by.school, .(SCHOOLID), transform, YTD=cumsum(N))
  
  cum.monthly[SCHOOLID==78102,  School:= "KAP"]
  cum.monthly[SCHOOLID==7810,   School:="KAMS"]
  cum.monthly[SCHOOLID==400146, School:="KCCP"]
  cum.monthly[SCHOOLID==400163, School:="KBCP"]
  
  ####Need to build data fram that contains actual YTD, as well as 10% annual ceiling for each school.  Months need to 
  # start with October
  mons<-c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")
  mons<-factor(mons, levels=mons, ordered=TRUE)

  cum.monthly[,Month:=factor(Month, levels=mons)]
  
  #School<-c("KAPS", "KAMS", "KCCP")
  Schools<-unique(as.character(cum.monthly$School))
  Schools<-factor(Schools, levels=c("KAP", "KAMS","KCCP", "KBCP" ))
  
  cum.monthly[,School:=factor(School, levels=c("KAP", "KAMS","KCCP", "KBCP" ))]

  redline<-enrolled.data[,list(N.Enrolled=.N), by=SCHOOLID]
  #redline<-ddply(enrolled.data, .(SCHOOLID), summarise, N.Enrolled=length(STUDENTID))
  redline[,N.10pct:=N.Enrolled*.1]
  redline[,line.month:=N.10pct/12]
  redline[SCHOOLID==78102,  School:= "KAP"]
  redline[SCHOOLID==7810,   School:="KAMS"]
  redline[SCHOOLID==400146, School:="KCCP"]
  redline[SCHOOLID==400163, School:="KBCP"]
  redline<-redline[,list(School, line.month)]
  
  xferplot.data<-expand.grid(Month=mons, School=Schools, Variable=c("Cumulative Transfers", "Ceiling"))
  xferplot.data<-merge(x=xferplot.data, y=redline, by="School",all.x=TRUE )
  names(xferplot.data)[4]<-"Value"
  xferplot.data$Value[xferplot.data$Variable=="Cumulative Transfers"]<-NA
  
  xfer.cums<-cum.monthly[,list(School, Month, N)]
  #xfer.cums<-cum.monthly[,c("School", "Month", "N")]
  xfer.cums[,Variable:="Cumulative Transfers"]
  
  
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