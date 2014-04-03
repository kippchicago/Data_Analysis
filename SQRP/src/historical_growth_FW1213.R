#### Growth percentages from last year
#require(ProjectTemplate)
#setwd("~/Dropbox/Consulting/KIPP Ascend/Data Analysis/SQRP")
#load.project()


info(logger, "Match Data Fall-Winter 2012-13")
m.fw.13<-s2s_match(map.all, 
          season1 = "Fall", 
          season2 = "Winter",
          sy=2013)

m.fs.13<-s2s_match(map.all, 
                   season1 = "Fall", 
                   season2 = "Spring",
                   sy=2013)

m.ws.13<-s2s_match(map.all, 
                   season1 = "Winter", 
                   season2 = "Spring",
                   sy=2013)

info(logger, "Convert to data.table")
m.fw.13<-data.table(m.fw.13)
m.fs.13<-data.table(m.fs.13)
m.ws.13<-data.table(m.ws.13)

info(logger, "Calc summaries.")
m.fw.13.s<-s2s_summary(m.fw.13, school = c("KAP", "KAMS", "KCCP"))
m.fs.13.s<-s2s_summary(m.fs.13, school = c("KAP", "KAMS", "KCCP"))
m.ws.13.s<-s2s_summary(m.ws.13, school = c("KAP", "KAMS", "KCCP"))

#m.fw.13.s[,fw_multi:=avg_term_2/avg_term_1]
#m.fs.13.s[,fs_multi:=avg_winter/avg_fall]
#m.ws.13.s[,ws_multi:=avg_winter/avg_fall]


m.fws.s<-inner_join(m.fw.13.s, m.ws.13.s, by=c("SchoolInitials.x", "MeasurementScale", "Grade.x")) %.% mutate(mult=(diff.y+diff.x)/diff.x)
setnames(m.fws.s, colnames(m.fws.s), str_replace(colnames(m.fws.s), "\\.x", ".fw"))
setnames(m.fws.s, colnames(m.fws.s), str_replace(colnames(m.fws.s), "\\.y", ".ws"))
setnames(m.fws.s, c("SchoolInitials.fw", "Grade.fw"), c("School", "Grade"))

m.fws.s<-data.table(m.fws.s)

#m.table<-m.fws.s[N.fw>20, list(School, MeasurementScale, Grade, "FW RIT Growth"=diff.fw, "WS RIT Growth"=diff.ws, "Multiplier (WS/FW)=mult)]
m.table<-m.fws.s[N.fw>20, list(School, MeasurementScale, Grade, diff.fw, diff.ws, mult)]
