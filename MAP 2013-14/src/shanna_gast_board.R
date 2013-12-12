#  Analysis for Board deck by Shanna Gast

seasons<-c("Spring09", "Spring10", "Spring11", "Spring12", "Spring13")

mapresults.list <- lapply(seasons, get_MAPResults, connection=con, season2=NULL)

mapresults.list2 <- mapply(cbind, mapresults.list, "TestTerm"=seasons, SIMPLIFY=FALSE)

mapresults.dt <- as.data.table(rbindlist(mapresults.list2))

maxgrades <- mapresults.dt[,list(Grade=max(Grade)), by=list(TestTerm, SchoolName)]

setkeyv(maxgrades, c("TestTerm", "SchoolName", "Grade"))
setkeyv(mapresults.dt, c("TestTerm", "SchoolName", "Grade"))

GT50pctl.dt<-mapresults.dt[maxgrades][
      MeasurementScale %in% c("Reading", "Mathematics"),
      list(PctGT50=round(sum(TestPercentile>=50, na.rm=T)/.N*100,1)), 
      by=list(TestTerm, MeasurementScale, Grade)][
        order(MeasurementScale,Grade,TestTerm)]

allGT50pctl.dt<-mapresults.dt[maxgrades][
  MeasurementScale %in% c("Reading", "Mathematics"),
  list(PctGT50=round(sum(TestPercentile>=50, na.rm=T)/.N*100,1)), 
  by=list(TestTerm, MeasurementScale)][
    order(MeasurementScale,TestTerm)]