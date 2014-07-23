# data for KCCP 
setwd("~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP 2013-14")
map.db<-as.data.frame(read.dcf('config//mapdb.dcf'))
mapsrc<-src_mysql(dbname=as.character(map.db$dbname),
host=as.character(map.db$host),
user=as.character(map.db$user),
password=as.character(map.db$password))

map.all<-collect(tbl(mapsrc, "viewAllAssessments"))

hrs<-collect(tbl(mapsrc, "tblClassAssignmentsSpring14"))

map.all<-left_join(map.all, select(hrs, TermName, StudentID, ClassName), 
                   by=c("TermName", "StudentID"))

hrs_kccp<-filter(hrs, SchoolName=="KIPP Create College Prep")

hrs_kccp<-unique(hrs_kccp$ClassName)

hrs_kccp<-hrs_kccp[order(hrs_kccp)]

map.mv<-mapvizier(map.all)

strands_plot(map.mv, Grade==5, ClassName=="5th Columbia", MeasurementScale=="Mathematics", Year2==2014, Season=="Spring" )

p<-list()

for(cr in hrs_kccp) {
  for(ms in c("Mathematics", "Reading", "General Science")) {
  p[[paste(cr, ms)]]<-
    strands_plot(map.mv, 
               ClassName==cr, 
               MeasurementScale==ms, 
               SchoolInitials=="KCCP",
               Year2==2014, 
               Season=="Spring" 
               ) +  ggtitle(paste("KCCP", cr, ms, sep=" | "))
  }
}

cairo_pdf("graphs/strands_kccp.pdf", height=8.25, width=10.75, onefile = TRUE)
  p
dev.off()  
  

# Now write the CSV
kccp<-filter(map.mv$mapData, Season=="Spring", Year2==2014, SchoolInitials=="KCCP")

glimpse(kccp)
write.csv(kccp, file="reports/kccp_cdf.csv")

  