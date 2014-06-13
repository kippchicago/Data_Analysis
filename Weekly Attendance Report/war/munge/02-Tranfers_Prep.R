xferplot.1213<-transferplot.data.prep(Xfers.HSR.1213, Enrolled.121003)
xferplot.1314<-transferplot.data.prep(Xfers.HSR.1314, Enrolled.131001)

xferplot.1213$Year<-"SY12-13"
xferplot.1314$Year<-"SY13-14"

xferplot<-rbind(xferplot.1213, xferplot.1314)


#non movers
xferplot.1213.nonmovers<-transferplot.data.prep(filter(Xfers.HSR.1213, EXITCODE!=2), Enrolled.121003)
xferplot.1314.nonmovers<-transferplot.data.prep(filter(Xfers.HSR.1314, EXITCODE!=2), Enrolled.131001)

xferplot.1213.nonmovers$Year<-"SY12-13"
xferplot.1314.nonmovers$Year<-"SY13-14"


xferplot.nm<-rbind(xferplot.1213.nonmovers, xferplot.1314.nonmovers)
