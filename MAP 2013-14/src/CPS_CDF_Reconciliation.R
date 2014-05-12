Ascend.CPS.CDF
Ascend.KIPPChi.cdf<-copy(map.W14)

Ascend.KIPPChi.cdf[,SchoolInitials:=abbrev(SchoolName, list(old="KAPS", new="KAP"))]

Ascend.KIPPChi.cdf[Subject %in% c("Reading", "Mathematics") & 
                     SchoolInitials %in% c("KAMS")][,.N,by=list(Winter14_Grade, Subject)]

x<-unique(Ascend.KIPPChi.cdf[SchoolInitials=="KAP" &
                            Winter14_Grade>=2, list(StudentID, StudentLastName, StudentFirstName, Winter14_Grade)])[order(Winter14_Grade, StudentLastName, StudentFirstName)]

write.csv(x, file="reports/KAP_CPS_CDF_missing.csv")
