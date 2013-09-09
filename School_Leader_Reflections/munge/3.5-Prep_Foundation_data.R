kc.grades<-c(0:2,5:8)
kc.schools<-c("KIPP Create Middle School", "KIPP Ascend Middle School", "KIPP Ascend Primary School", "KIPP Network", "National Norm")


MAP.KIPP.Network.1213[,Perc_Growth:=as.numeric(gsub("(\\d+)%", "\\1", Perc_Growth))]



MAP.KIPP.Network.1213[, Col:=School_Display_Name %in% kc.schools]
MAP.KIPP.Network.1213[Col==TRUE, School_Display_Name_Chi:=School_Display_Name]
MAP.KIPP.Network.1213[Col==FALSE, School_Display_Name_Chi:="KIPP"]

