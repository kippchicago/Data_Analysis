
## ----prereqs-------------------------------------------------------------
setwd("~/Dropbox (KIPP Chicago Schools)/Data Analysis/MAP Analysis/2014-2015")
require(ProjectTemplate)
load.project()
require(assertthat)
map_fall14<-map_all %>% filter(TermName=="Fall 2014-2015") %>%
  left_join(select(map_homerooms, StudentID, ClassName), by="StudentID")

map_f14<-mapvizier(map_fall14)


map_filtered<-map_f14$mapData %>% 
         filter(Grade==6, 
                MeasurementScale=="Mathematics") %>%
       select(ClassName, SchoolInitials) 


## ----plot_goal_strands, echo=FALSE, fig.width=10.75, fig.height=8.25-----
for(s in unique(map_filtered$SchoolInitials)){
  
  for(cn in unique(filter(map_filtered, SchoolInitials==s)$ClassName)){

    message(paste("Plotting strand plot for", cn, "\n at", s))
    p<-strands_plot(map_f14, ClassName==cn, SchoolInitials==s, MeasurementScale=="Mathematics")
    p<-p +  ggtitle(paste("MAP Goal Strands Deviations\n", s, cn))
    print(p)
  }
}


## ----plot_strand_lists, fig.height=8, fig.width=10.5, echo=FALSE---------

for(s in unique(map_filtered$SchoolInitials)){
  for(g in unique(filter(map_f14$mapData, SchoolInitials==s)$Grade)) {
    for(ms in c("Mathematics", "Reading")) {
     message(paste("Plotting strand list plot for",
                   g,
                   ms,
                   "\n at", 
                   s))
    p<-strands_list_plot(map_f14,
                        Grade==g,
                        SchoolInitials==s,
                        MeasurementScale==ms)
    p<-p +  ggtitle(paste("MAP Goal Strand\nStudents by Strand\n", 
                          s, 
                          g, 
                          ms
                          )
                    )
    print(p) 
    }
  }
}


