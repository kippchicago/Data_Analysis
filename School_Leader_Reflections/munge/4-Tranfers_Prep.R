xferplot.1213<-transferplot.data.prep(Xfers.HSR.1213, Enrolled.121003)
xferplot.1112<-transferplot.data.prep(Xfers.HSR.1112, Enrolled.110930)


#table of Transfers by schoool 

Xfersreasons<-data.table(data.frame(EXITCODE=as.character(c(1:11,99)),Reason=c("Dropped Out", "Moved", "Transport", "Expelled", "Behavior/Discipline", "Academics", "Avoid Retention", "Special Needs", "Other", "Don't Know", "Xfer Other KIPP", "DNA")))

setkey(Xfers.HSR.1213, EXITCODE)
setkey(Xfersreasons, EXITCODE)

Xfer.table<-copy(Xfersreasons[Xfers.HSR.1213])

Xfer.table<-cast( ddply(Xfer.table, .(Reason, SCHOOLID), function(df)c(N=nrow(df))), Reason~SCHOOLID, margins=TRUE, fun.aggregate=sum)
names(Xfer.table)<-c("Tranfer Reason", "KAMS", "KAPS", "KCCP", "Region")
levels(Xfer.table[,1])[13]<-"Total"
Xfer.table<-Xfer.table[,c("Tranfer Reason", "KAPS", "KAMS", "KCCP", "Region")]
g.tbl<-tableGrob(xtable(Xfer.table), show.rownames=FALSE)