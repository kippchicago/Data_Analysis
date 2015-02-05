#
require(ProjectTemplate)
load.project()

map_mv<-mapvizier(map_all_silo)

schools<-c("KACP", "KCCP", "KBCP")

p_pctl<-list()

start_term<-c("Spring 2013-2014", "Fall 2014-2015")

for(term in start_term){
  term2 <- str_replace(term, " ", "_")
  for(s in schools){
    for(ms in c("Reading", "Mathematics", "General Science")){
      grades <- unique(filter(map_mv$mapData, SchoolInitials==s)$Grade)
      grades<-grades[order(grades)]
      grades<-grades[1:length(grades)-1]
      
      for (grade in grades){
        grade2<-grade
        if(term == "Spring 2013-2014") grade2=grade+1
        p<-  pctl_change_plot(map_mv, 
                              term, 
                              "Winter 2014-2015", 
                              n_col=7, 
                              min_n=5,
                              MeasurementScale==ms, 
                              Grade %in% c(grade,grade2), 
                              SchoolInitials==s
        ) 
        if(!is.null(p)){
          p_pctl[[paste(s,grade2,str_replace(ms, " ", "_"),term2, sep="_")]]<- 
            p + ggtitle(paste(term, "to Winter 2014-2015 NPR Losses/Gains\n",
                              s,
                              "Current Grade:",
                              grade2,
                              ms
            )
            ) +
            theme(strip.text=element_text(size=7),
                  axis.text.x=element_text(size=5))
          message(paste("Plotting:", s, grade2, ms, term))
        }
      }
    }  
  }
}



pdf(file = "graphs//Winter_Pctl_Change_Plots_KAP.pdf", height=10.75, width=9.25, onefile = TRUE)
p_pctl[names(p_pctl)[grepl("KACP_[^6-8]", names(p_pctl))]]
dev.off()

pdf(file = "graphs//Winter_Pctl_Change_Plots_KAP.pdf", height=10.75, width=9.25, onefile = TRUE)
p_pctl[names(p_pctl)[grepl("KACP_[6-8]", names(p_pctl))]]
dev.off()


pdf(file = "graphs//Winter_Pctl_Change_Plots_KCCP.pdf", height=10.75, width=9.25, onefile = TRUE)
p_pctl[names(p_pctl)[grepl("KCCP", names(p_pctl))]]
dev.off()


pdf(file = "graphs//Winter_Pctl_Change_Plots_KBCP.pdf", height=10.75, width=9.25, onefile = TRUE)
 p_pctl[names(p_pctl)[grepl("KBCP", names(p_pctl))]]
dev.off()
