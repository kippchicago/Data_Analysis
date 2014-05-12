library(ProjectTemplate)
load.project()

FW.dt<-PrepMAP(map.F13W14, season1="Fall13", season2="Winter14")
awards<-copy(FW.dt)

awards[,StandardizedGrowth:=((Winter14_RIT-Fall13_RIT)-TypicalFallToWinterGrowth)/SDFallToWinterGrowth]

# By classroom ####
#Typical
awards[SchoolInitials=="KAP",list(D=sum(Winter14_RIT>=ProjectedGrowth, na.rm=T)/.N), by=list(SchoolInitials, Subject, Winter14_Grade, Winter14_Classname)][order(-D)]
awards[SchoolInitials!="KAP",list(D=sum(Winter14_RIT>=ProjectedGrowth, na.rm=T)/.N), by=list(SchoolInitials, Subject, Winter14_Grade, Winter14_Classname)][order(-D)]

#CR
awards[SchoolInitials=="KAP",list(D=sum(Winter14_RIT>=CollegeReadyGrowth, na.rm=T)/.N), by=list(SchoolInitials, Subject, Winter14_Grade, Winter14_Classname)][order(-D)]
awards[SchoolInitials!="KAP",list(D=sum(Winter14_RIT>=CollegeReadyGrowth, na.rm=T)/.N), by=list(SchoolInitials, Subject, Winter14_Grade, Winter14_Classname)][order(-D)]

#50th
awards[SchoolInitials=="KAP",list(D=sum(Winter14_Pctl>=50, na.rm=T)/.N), by=list(SchoolInitials, Subject, Winter14_Grade, Winter14_Classname)][order(-D)]
awards[SchoolInitials!="KAP",list(D=sum(Winter14_Pctl>=50, na.rm=T)/.N), by=list(SchoolInitials, Subject, Winter14_Grade, Winter14_Classname)][order(-D)]

#75th
awards[SchoolInitials=="KAP",list(D=sum(Winter14_Pctl>=75, na.rm=T)/.N), by=list(SchoolInitials, Subject, Winter14_Grade, Winter14_Classname)][order(-D)]
awards[SchoolInitials!="KAP",list(D=sum(Winter14_Pctl>=75, na.rm=T)/.N), by=list(SchoolInitials, Subject, Winter14_Grade, Winter14_Classname)][order(-D)]

#1 Quartile
awards[SchoolInitials=="KAP",list(D=(sum(Fall13_Quartile==1 & Winter14_Quartile==2, na.rm=T)+sum(Fall13_Quartile==2 & Winter14_Quartile==3, na.rm=T) + sum(Fall13_Quartile==3 & Winter14_Quartile==4, na.rm=T))/.N), by=list(SchoolInitials, Subject, Winter14_Grade, Winter14_Classname)][order(-D)]
awards[SchoolInitials!="KAP",list(D=(sum(Fall13_Quartile==1 & Winter14_Quartile==2, na.rm=T)+sum(Fall13_Quartile==2 & Winter14_Quartile==3, na.rm=T) + sum(Fall13_Quartile==3 & Winter14_Quartile==4, na.rm=T))/.N), by=list(SchoolInitials, Subject, Winter14_Grade, Winter14_Classname)][order(-D)]

#2 Quartile
awards[SchoolInitials=="KAP",list(D=(sum(Fall13_Quartile==1 & Winter14_Quartile==3, na.rm=T)+sum(Fall13_Quartile==2 & Winter14_Quartile==4, na.rm=T))/.N), by=list(SchoolInitials, Subject, Winter14_Grade, Winter14_Classname)][order(-D)]
awards[SchoolInitials!="KAP",list(D=(sum(Fall13_Quartile==1 & Winter14_Quartile==3, na.rm=T)+sum(Fall13_Quartile==2 & Winter14_Quartile==4, na.rm=T))/.N), by=list(SchoolInitials, Subject, Winter14_Grade, Winter14_Classname)][order(-D)]



# By Grade ####
#Typical
awards[SchoolInitials=="KAP",list(D=sum(Winter14_RIT>=ProjectedGrowth, na.rm=T)/.N), by=list(SchoolInitials, Subject, Winter14_Grade)][order(-D)]
awards[SchoolInitials!="KAP",list(D=sum(Winter14_RIT>=ProjectedGrowth, na.rm=T)/.N), by=list(SchoolInitials, Subject, Winter14_Grade)][order(-D)]

#CR
awards[SchoolInitials=="KAP",list(D=sum(Winter14_RIT>=CollegeReadyGrowth, na.rm=T)/.N), by=list(SchoolInitials, Subject, Winter14_Grade)][order(-D)]
awards[SchoolInitials!="KAP",list(D=sum(Winter14_RIT>=CollegeReadyGrowth, na.rm=T)/.N), by=list(SchoolInitials, Subject, Winter14_Grade)][order(-D)]

#50th
awards[SchoolInitials=="KAP",list(D=sum(Winter14_Pctl>=50, na.rm=T)/.N), by=list(SchoolInitials, Subject, Winter14_Grade)][order(-D)]
awards[SchoolInitials!="KAP",list(D=sum(Winter14_Pctl>=50, na.rm=T)/.N), by=list(SchoolInitials, Subject, Winter14_Grade)][order(-D)]

#75th
awards[SchoolInitials=="KAP",list(D=sum(Winter14_Pctl>=75, na.rm=T)/.N), by=list(SchoolInitials, Subject, Winter14_Grade)][order(-D)]
awards[SchoolInitials!="KAP",list(D=sum(Winter14_Pctl>=75, na.rm=T)/.N), by=list(SchoolInitials, Subject, Winter14_Grade)][order(-D)]

#1 Quartile
awards[SchoolInitials=="KAP",list(D=(sum(Fall13_Quartile==1 & Winter14_Quartile==2, na.rm=T)+sum(Fall13_Quartile==2 & Winter14_Quartile==3, na.rm=T) + sum(Fall13_Quartile==3 & Winter14_Quartile==4, na.rm=T))/.N), by=list(SchoolInitials, Subject, Winter14_Grade)][order(-D)]
awards[SchoolInitials!="KAP",list(D=(sum(Fall13_Quartile==1 & Winter14_Quartile==2, na.rm=T)+sum(Fall13_Quartile==2 & Winter14_Quartile==3, na.rm=T) + sum(Fall13_Quartile==3 & Winter14_Quartile==4, na.rm=T))/.N), by=list(SchoolInitials, Subject, Winter14_Grade)][order(-D)]

#2 Quartile
awards[SchoolInitials=="KAP",list(D=(sum(Fall13_Quartile==1 & Winter14_Quartile==3, na.rm=T)+sum(Fall13_Quartile==2 & Winter14_Quartile==4, na.rm=T))/.N), by=list(SchoolInitials, Subject, Winter14_Grade)][order(-D)]
awards[SchoolInitials!="KAP",list(D=(sum(Fall13_Quartile==1 & Winter14_Quartile==3, na.rm=T)+sum(Fall13_Quartile==2 & Winter14_Quartile==4, na.rm=T))/.N), by=list(SchoolInitials, Subject, Winter14_Grade)][order(-D)]

awards[SchoolInitials!="KAP",list(D=mean(StandardizedGrowth), Pctl=pnorm(mean(StandardizedGrowth)), Pctl2=mean(pnorm(StandardizedGrowth))), 
                                  by=list(SchoolInitials, Subject, Winter14_Grade)][order(-D)]

