
## ----prereqs-------------------------------------------------------------
setwd("~/Dropbox (KIPP Chicago Schools)/Data Analysis/MAP Analysis/2014-2015")
require(ProjectTemplate)
load.project()
require(assertthat)

map_winter15<-map_all_silo %>% filter(TermName=="Winter 2014-2015") %>%
  left_join(select(map_homerooms, StudentID, ClassName, TermName), 
            by=c("StudentID", "TermName")
  ) %>%
  rename(ClassName=ClassName.y) %>%
  select(-ClassName.x)

map_w15<-mapvizier(map_winter15)

schools<-unique(map_w15$mapData$SchoolInitials)

## ----plot_goal_strands, echo=FALSE, fig.width=10.75, fig.height=8.25-----
require(stringr)
p_strands<-list()
for(s in schools){
  for(g in unique(filter(map_w15$mapData, SchoolInitials==s)$Grade)) {
    for(cn in unique(filter(map_w15$mapData, SchoolInitials==s, Grade==g)$ClassName)){
      for(ms in c("Mathematics", "Reading", "General Science")) {
        message(paste("Plotting strand plot for", cn, "\n at", s))
        p<-strands_plot(map_w15, ClassName==cn, SchoolInitials==s, MeasurementScale==ms)
        cn2<-str_replace(cn, " ",  "_")
        if(inherits(p, c("text", "grob"))) {
          p_strands[[paste(s, cn2, ms, sep="_")]] <- p 
        } else {
          p_strands[[paste(s, cn2, ms, sep="_")]] <-
            p +  ggtitle(paste("MAP Goal Strands Deviations\n", s, cn, ms))
        }        
      }
    }
  }
}


pdf(file = "graphs//Winter_Strand_Deviation_Plots.pdf", height=10.75, width=9.25, onefile = TRUE)
  p_strands
dev.off()


pdf(file = "graphs//Winter_Strand_Deviation_Plots_KAP.pdf", height=10.75, width=9.25, onefile = TRUE)
p_strands[names(p_strands)[grepl("KACP_[^6-8]", names(p_strands))]]
dev.off()

pdf(file = "graphs//Winter_Strand_Deviation_Plots_KAMS.pdf", height=10.75, width=9.25, onefile = TRUE)
p_strands[names(p_strands)[grepl("KACP_[6-8]", names(p_strands))]]
dev.off()


pdf(file = "graphs//Winter_Strand_Deviation_Plots_KCCP.pdf", height=10.75, width=9.25, onefile = TRUE)
  p_strands[names(p_strands)[grepl("KCCP", names(p_strands))]]
dev.off()

pdf(file = "graphs//Winter_Strand_Deviation_Plots_KBCP.pdf", height=10.75, width=9.25, onefile = TRUE)
  p_strands[names(p_strands)[grepl("KCCP", names(p_strands))]]
dev.off()






## ----plot_strand_lists, fig.height=8, fig.width=10.5, echo=FALSE---------
p<-list()
for(s in schools){
  for(g in unique(filter(map_w15$mapData, SchoolInitials==s)$Grade)) {
    for(ms in c("Mathematics", "Reading", "General Science")) {
     message(paste("Plotting strand list plot for",
                   g,
                   ms,
                   "\n at", 
                   s))
    p1<-strands_list_plot(map_w15,
                        Grade==g,
                        SchoolInitials==s,
                        MeasurementScale==ms)
    
    if(inherits(p1, c("text", "grob"))) { 
      p[[paste(s,g,ms, sep="_")]] <- p1
    } else {
      p[[paste(s,g,ms, sep="_")]] <-
        p1 +  ggtitle(paste("MAP Goal Strand\nStudents by Strand\n", 
                          s, 
                          g, 
                          ms
                          )
                    ) 
    }
    }
  }
}


pdf(file = "graphs//Winter_Strand_Plots.pdf", height=10.75, width=9.25, onefile = TRUE)
  p
dev.off()


