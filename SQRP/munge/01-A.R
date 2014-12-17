# Munging script for map.norms

setnames(map.norms, old="StartGrade2", new="Grade")

#info(logger, "Merging ReportedWinterToSpringGrowth with map.all")

ws.norms<-select(map.norms, Grade=as.integer(Grade), MeasurementScale, StartRIT ,TypicalWinterToSpringGrowth=R12)

ws.norms$Grade<-as.integer(ws.norms$Grade)

setnames(ws.norms, "StartRIT", "TestRITScore")

#info(logger, "Adding TypicalWinterToSpringGrowth")

map.all<-left_join(as.data.frame(map.all), ws.norms, by=c("Grade", "MeasurementScale", "TestRITScore"))

map.all<-data.table(map.all)
