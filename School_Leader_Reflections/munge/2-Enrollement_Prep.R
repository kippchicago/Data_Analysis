Enrollment.table<-ddply(Enrollment, .(GRADE), summarise, Enrolled=sum(MEMBERSHIPSHARE),PctMale=sum(GENDER=="M")/sum(MEMBERSHIPSHARE), PctFemale=sum(GENDER=="F")/sum(MEMBERSHIPSHARE),PctBlack=sum(ETHNICITY_CODE==2)/sum(MEMBERSHIPSHARE), PctLatino=sum(ETHNICITY_CODE==5)/sum(MEMBERSHIPSHARE),PctFRM=sum(FRL, na.rm=TRUE)/sum(MEMBERSHIPSHARE), PctELL=sum(ELL, na.rm=TRUE)/sum(MEMBERSHIPSHARE), PctSPED=sum(SPED, na.rm=TRUE)/sum(MEMBERSHIPSHARE)) 

attach(Enrollment)
Enrollment.table.totals<-data.frame(GRADE="Total",Enrolled=sum(MEMBERSHIPSHARE),PctMale=sum(GENDER=="M")/sum(MEMBERSHIPSHARE), PctFemale=sum(GENDER=="M")/sum(MEMBERSHIPSHARE),PctBlack=sum(ETHNICITY_CODE==2)/sum(MEMBERSHIPSHARE), PctLatino=sum(ETHNICITY_CODE==5)/sum(MEMBERSHIPSHARE),PctFRM=sum(FRL, na.rm=TRUE)/sum(MEMBERSHIPSHARE), PctELL=sum(ELL, na.rm=TRUE)/sum(MEMBERSHIPSHARE), PctSPED=sum(SPED, na.rm=TRUE)/sum(MEMBERSHIPSHARE))
detach(Enrollment)

Enrollment.table<-rbind(Enrollment.table, Enrollment.table.totals)

Enrollment.table[,c(3:9)]<-round(Enrollment.table[,c(3:9)]*100,0)

names(Enrollment.table)<-c("Grade", "Enrollment", "%\nMale", "%\nFemale", "%\nBlack", "%\nLatino", "%\nFRM", "%\nELL", "%\nSPED")