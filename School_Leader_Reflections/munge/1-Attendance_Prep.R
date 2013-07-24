Attendence[SCHOOLID==7810,SchoolInitials:="KAMS"]
Attendence[SCHOOLID==78102,SchoolInitials:="KAPS"]
Attendence[SCHOOLID==400146,SchoolInitials:="KCCP"]          
           
        
Attendence$SchoolInitials<-factor(Attendence$SchoolInitials, levels=c("KAPS", "KAMS", "KCCP"))



cache('Attendence')