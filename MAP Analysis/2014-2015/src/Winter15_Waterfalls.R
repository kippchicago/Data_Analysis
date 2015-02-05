setwd("~/Dropbox (KIPP Chicago Schools)/Data Analysis/MAP Analysis/2014-2015")
require(ProjectTemplate)
load.project()
require(assertthat)



map_mv<-mapvizier(map_all_silo)

# test



schools <- c("KACP", "KCCP", "KBCP")


require(stringr)

p_waterfalls<-list()


for(term in c("Spring - Winter", "Fall - Winter")){
  for(s in schools){
    grades<-unique(dplyr::filter(map_mv$seasonMatched, 
                    SchoolInitials==s,
                    Year2.2==2015,
                    GrowthSeason==term)$Grade.2
                   )
    for(g in grades) {
      measurementscales<-unique(filter(map_mv$seasonMatched,
                                SchoolInitials.2==s,
                                GrowthSeason==term)$MeasurementScale
                                )
      for(ms in measurementscales) {
        message(paste("Plotting waterfall  for", term, g, ms, "\n at", s))
        ms2<-str_replace(ms, " ",  "_")
        w2<-str_replace(term, " - ",  "_")
        
        p<-try(haid_plot(df=map_mv, 
                    SchoolInitials==s, 
                    Grade.2==g, 
                    MeasurementScale==ms, 
                    GrowthSeason==term, 
                    Year2.2==2015),
               silent=TRUE) #+
        if(!class(p)=="try-error"){
          p_waterfalls[[paste(s, g, ms2, w2, g, sep="_")]]<-p +  
            ggtitle(paste("MAP RIT Growth (Waterfall)\n", s, " | ", g," | ", ms," | ",term))
        }
        
        
        
        
      }    
    }
  }
}


pdf(file = "graphs//Winter_Waterfall_Plots.pdf", height=10.75, width=8.25, onefile = TRUE)
p_waterfalls
dev.off()


pdf(file = "graphs//Winter_Waterfall_Plots_KAP_by_grade.pdf", height=10.75, width=8.25, onefile = TRUE)
p_waterfalls[names(p_waterfalls)[grepl("KACP_[^6-8]", names(p_waterfalls))]]
dev.off()

pdf(file = "graphs//Winter_Waterfall_Plots_KAMS.pdf", height=10.75, width=8.25, onefile = TRUE)
p_waterfalls[names(p_waterfalls)[grepl("KACP_[6-8]", names(p_waterfalls))]]
dev.off()



pdf(file = "graphs//Winter_Waterfall_Plots_KCCP.pdf", height=10.75, width=8.25, onefile = TRUE)
p_waterfalls[names(p_waterfalls)[grepl("KCCP", names(p_waterfalls))]]
dev.off()

pdf(file = "graphs//Winter_Waterfall_Plots_KBCP.pdf", height=10.75, width=8.25, onefile = TRUE)
p_waterfalls[names(p_waterfalls)[grepl("KBCP", names(p_waterfalls))]]
dev.off()
