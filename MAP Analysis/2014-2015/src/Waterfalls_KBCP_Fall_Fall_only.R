# KBCP October PD daty Waterfall charts

# This one only uses fall 2014 scores
setwd("~/Dropbox (KIPP Chicago Schools)/Data Analysis/MAP Analysis/2014-2015")
require(ProjectTemplate)
load.project()


# Get KBCP only for fal and subset by those with a fall score but not spring socrel
# and change term name to Spring and add a start to last name to indicate we will use fall score
map_fall <- map_all %>% filter(TermName=="Fall 2014-2015", 
                               SchoolName=="KIPP Bloom College Prep")


# creat mapvizier object
map_mv<-mapvizier(map_fall)

year2 <- 2015
grade<-5
subj<-"Reading"
wf_title<-paste("Fall 2014 MAP RIT Scores (Percentiles) & Fall-to-Spring Goals\n",
                "KBCP", grade, subj, "by Fall Quartile")

p2<-haid_plot(map_mv,
              Grade==grade, 
              MeasurementScale==subj, 
              Year2==year2, 
              p_title = wf_title
)

p2

p<-list()
for(subj in c("Mathematics", "Reading", "General Science")){
  for(gr in c(5:6)){
    
    obj_name<-paste("KBCP", gr, subj)
    
    title<-paste("Fall 2014 MAP RIT Scores (Percentiles) & Fall-to-Spring Goals\n",
                    "KBCP", gr, subj
                 )
    
    p[[obj_name]] <- haid_plot(map_mv,
                               Grade==gr, 
                               MeasurementScale==subj, 
                               Year2==2015, 
                               p_title = title,
                               p_name_size=2
    )
  }
}

today<-today() %>% format(.,"%y%m%d")
cairo_pdf(file=paste0("graphs/KBCP_Waterfalls_Fall_fall_scores_only_", today, ".pdf"), 
          height = 8.25, 
          width=10.75, 
          onefile = TRUE)

p

dev.off()


