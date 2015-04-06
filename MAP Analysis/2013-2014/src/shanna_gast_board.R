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

ggplot(GT50pctl.dt, aes(x=TestTerm, y=PctGT50)) + 
  geom_line(data=GT50pctl.dt[Grade==8], aes(group=MeasurementScale)) + 
  facet_grid(.~MeasurementScale) + 
  geom_point(aes(color=as.factor(Grade), size=N))

# Write to CSV
write.csv(GT50pctl.dt, file="reports/Board_Percent_GT_50th_Pctl.csv")


#### Fall to Spring Analyses ####
pctgrowth.list <- mapply(get_MAPResults,
                         season1=c("Fall08", 
                                   "Fall09",
                                   "Fall10",
                                   "Fall11",
                                   "Fall12"), 
                         season2=c("Spring09", 
                                   "Spring10",
                                   "Spring11",
                                   "Spring12",
                                   "Spring13"), 
                         MoreArgs=list(connection=con), 
                         SIMPLIFY=F
                         )




pctgrowth.list<-lapply(pctgrowth.list, abstractSeasonNames)

growth_seasons <- c("Fall 2008 - Spring 2009",
                    "Fall 2009 - Spring 2010",
                    "Fall 2010 - Spring 2011",
                    "Fall 2011 - Spring 2012",
                    "Fall 2012 - Spring 2013")

pctgrowth.list <- mapply(cbind, pctgrowth.list, "GrowthSeason"=growth_seasons, SIMPLIFY=FALSE)

pctgrowth.dt<-as.data.table(rbindlist(pctgrowth.list))

pctgrowth.dt[, MetGrowth:=Season2_RIT-Season1_RIT>=ReportedFallToSpringGrowth]
pctgrowth.dt[Season1_Grade<=4, School:="KAP"]
pctgrowth.dt[Season1_Grade>4 & (SchoolName=="KIPP Ascend Charter School                                       "
            |SchoolName=="KIPP Ascend Middle School") , School:="KAMS"]

pctgrowth.dt[Season1_Grade>4 & SchoolName=="KIPP Create Middle School", School:="KCCP"]

pctgrowth.dt[,School:=factor(School, levels=c("KAP", "KAMS", "KCCP"))]

metgrowth.dt<-pctgrowth.dt[Subject %in% c("Reading", "Mathematics"), 
                           list(NumMEGrowth=sum(MetGrowth), 
                                TotalTestedFS=.N,
                                PctMETGrowth=round(sum(MetGrowth)/.N*100,1)), 
                           by=list(GrowthSeason, School, Season1_Grade, Subject)][
                          order(School, Subject, Season1_Grade, GrowthSeason)]


dodgewidth<-position_dodge(width=.9)
ggplot(metgrowth.dt, aes(x=GrowthSeason, y=PctMETGrowth, group=School)) +
  geom_bar(aes(fill=School), stat="identity", position=dodgewidth) + 
  geom_text(aes(label=PctMETGrowth), position=dodgewidth, vjust=-.5) + 
  facet_grid(Season1_Grade~Subject) + 
  theme(axis.text.x=element_text(angle=45, vjust=.5)) + 
  ylim(c(0,100))

write.csv(metgrowth.dt, file="reports/Board_Percent_Met_Exceed_Typical_Growth.csv")

