#Typical
awards[SchoolInitials=="KAP",list(D=sum(Winter14_RIT>=ProjectedGrowth, na.rm=T)/.N), by=list(SchoolInitials, Subject, Winter14_Grade, Winter14_Classname)][order(-D)]

#CR
awards[SchoolInitials=="KAP",list(D=sum(Winter14_RIT>=CollegeReadyGrowth, na.rm=T)/.N), by=list(SchoolInitials, Subject, Winter14_Grade, Winter14_Classname)][order(-D)]

#50th
awards[SchoolInitials=="KAP",list(D=sum(Winter14_Pctl>=50, na.rm=T)/.N), by=list(SchoolInitials, Subject, Winter14_Grade, Winter14_Classname)][order(-D)]

#75th
awards[SchoolInitials=="KAP",list(D=sum(Winter14_Pctl>=75, na.rm=T)/.N), by=list(SchoolInitials, Subject, Winter14_Grade, Winter14_Classname)][order(-D)]
#1 Quartile
awards[SchoolInitials=="KAP",list(D=(sum(Fall13_Quartile==1 & Winter14_Quartile==2, na.rm=T)+sum(Fall13_Quartile==2 & Winter14_Quartile==3, na.rm=T) + sum(Fall13_Quartile==3 & Winter14_Quartile==4, na.rm=T))/.N), by=list(SchoolInitials, Subject, Winter14_Grade, Winter14_Classname)][order(-D)]

#2 Quartile
awards[SchoolInitials=="KAP",list(D=(sum(Fall13_Quartile==1 & Winter14_Quartile==3, na.rm=T)+sum(Fall13_Quartile==2 & Winter14_Quartile==4, na.rm=T))/.N), by=list(SchoolInitials, Subject, Winter14_Grade, Winter14_Classname)][order(-D)]
