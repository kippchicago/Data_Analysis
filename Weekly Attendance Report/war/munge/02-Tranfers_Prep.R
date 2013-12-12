xferplot.1213<-transferplot.data.prep(Xfers.HSR.1213, Enrolled.121003)
xferplot.1314<-transferplot.data.prep(Xfers.HSR.1314, Enrolled.131001)

xferplot.1213$Year<-"SY12-13"
xferplot.1314$Year<-"SY13-14"

xferplot<-rbind(xferplot.1213, xferplot.1314)
