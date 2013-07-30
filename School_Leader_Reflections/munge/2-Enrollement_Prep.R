Enrollment.table<-ddply(Enrollment, .(GRADE), summarise, Enrolled=sum(MEMBERSHIPSHARE),PctMale=sum(GENDER=="M")/sum(MEMBERSHIPSHARE), PctFemale=sum(GENDER=="F")/sum(MEMBERSHIPSHARE),PctBlack=sum(ETHNICITY_CODE==2)/sum(MEMBERSHIPSHARE), PctLatino=sum(ETHNICITY_CODE==5)/sum(MEMBERSHIPSHARE),PctFRM=sum(FRL, na.rm=TRUE)/sum(MEMBERSHIPSHARE), PctELL=sum(ELL, na.rm=TRUE)/sum(MEMBERSHIPSHARE), PctSPED=sum(SPED, na.rm=TRUE)/sum(MEMBERSHIPSHARE)) 

attach(Enrollment)
Enrollment.table.totals<-data.frame(GRADE="Total",Enrolled=sum(MEMBERSHIPSHARE),PctMale=sum(GENDER=="M")/sum(MEMBERSHIPSHARE), PctFemale=sum(GENDER=="M")/sum(MEMBERSHIPSHARE),PctBlack=sum(ETHNICITY_CODE==2)/sum(MEMBERSHIPSHARE), PctLatino=sum(ETHNICITY_CODE==5)/sum(MEMBERSHIPSHARE),PctFRM=sum(FRL, na.rm=TRUE)/sum(MEMBERSHIPSHARE), PctELL=sum(ELL, na.rm=TRUE)/sum(MEMBERSHIPSHARE), PctSPED=sum(SPED, na.rm=TRUE)/sum(MEMBERSHIPSHARE))
detach(Enrollment)

Enrollment.table<-rbind(Enrollment.table, Enrollment.table.totals)

Enrollment.table[,c(3:9)]<-round(Enrollment.table[,c(3:9)]*100,0)

names(Enrollment.table)<-c("Grade", "Enrollment", "%\nMale", "%\nFemale", "%\nBlack", "%\nLatino", "%\nFRM", "%\nELL", "%\nSPED")


Enrollment.by.date<-Attendence[,.N, by=list(SchoolInitials,Grade=GRADE_LEVEL,CALENDARDATE)]

Enrollment.by.date[,Date:=ymd_hms(CALENDARDATE)]


Enrollment.by.date[, Month:=month(Date)]


Enrollment.by.date[,.SD[max(Date)], by=list(SchoolInitials,Month), mult="first"]

schooleyear<-interval(ymd("2012-8-15"),ymd("2013-06-13"))

Enrollment.plotdata<-Enrollment.by.date[Date %within% schooleyear,
                                        .SD[Date==max(Date),list(Date,N)], 
                                        by=list(SchoolInitials, Grade, Month)]

Enrollment.budgeted <- data.table(SchoolInitials=c(rep("KAPS",3), rep("KAMS",4), "KCCP"), Grade=c(0:2,5:8,5), Budget=c(104,104,104,90,90,85,80,80))