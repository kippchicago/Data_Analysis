# Script to get Winter Data 

require(ProjectTemplate)
load.project()

info(logger, "Prepping F13-W14 data")
FW.dt<-PrepMAP(map.F13W14, season1="Fall13", season2="Winter14")

info(logger, "Print middle Waterfall PDFs by grade.")



#Middle Schools first (since they need students by grade)
schools=list("KAMS", "KCCP", "KBCP")



lapply(schools, 
       pdf_waterfall, 
       .data=FW.dt,
       .by="grade",
       season1="Fall13", 
       season2="Winter14", 
       alpha=.6)
  

# Tabular summary for Winter 14 (should be abstracted and moved to liv)
tabSummaryMAP <- function(.data, school="KAMS"){
  dt<-copy(.data)
  dt.sum<-dt[SchoolInitials %in% school,
             list("Total Tested"= .N, 
                  "# >= Typical" = sum(Winter14_RIT>=ProjectedGrowth),  
                  "% >= Typical" = round(sum(Winter14_RIT>=ProjectedGrowth)/.N,2), 
                  "# >= College Ready" = sum(Winter14_RIT>=CollegeReadyGrowth),
                  "% >= Collge Ready" = round(sum(Winter14_RIT>=CollegeReadyGrowth)/.N,2)), 
             by=list(SchoolInitials, 
                     Winter14_Grade, 
                     Subject)]
  setnames(dt.sum, c("SchoolInitials", "Winter14_Grade"),
           c("School", "Grade"))
  
  dt.sum[order(School, Subject, Grade)]
}

lapply(schools, tabSummaryMAP, .data=FW.dt)

write.csv(tabSummaryMAP(FW.dt, "KAMS"), "reports/MAP_Winter_14_KAMS_.csv")
