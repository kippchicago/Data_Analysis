# KBCP October PD daty Waterfall charts

# This one has a wrinkle.

# We need to get spring scores for those that have it, and make them look like spring scores

# So we need to identify which kids have only fall scores from this sesssion and no spring 
# scrores from Spirng 2013-2014).  Select only them from Fall 2014-15, 
#change the term to Spring 2013-2014, and then bind them to the actual Spring 2013-2014.
# hopefully run mapvizier on that one season and feed it to haid_plot. Fingers crossed.

setwd("~/Dropbox (KIPP Chicago Schools)/Data Analysis/MAP Analysis/2014-2015")
require(ProjectTemplate)
load.project()

# Get KBCP only for spring (and icrement grade) and filter for those in 6-8 now
map_spring <- map_all %>% 
  filter(TermName=="Spring 2013-2014", 
         SchoolName=="KIPP Bloom College Prep") %>%
  mutate(Grade=Grade+1) %>%
  filter(Grade>=5)


# Get KBCP only for fal and subset by those with a fall score but not spring socrel
# and change term name to Spring and add a start to last name to indicate we will use fall score
map_fall <- map_all %>% filter(TermName=="Fall 2014-2015", 
                               SchoolName=="KIPP Bloom College Prep") %>%
  anti_join(map_spring, by=c("StudentID", "MeasurementScale")) %>%
  mutate(TermName = "Spring 2013-2014",
         StudentFirstname = paste("§", StudentFirstname))

# combine fall-replacement and spring
map_combined <- rbind(map_spring, map_fall)

# creat mapvizier object
map_mv<-mapvizier(map_combined)

year2 <- 2014
grade<-5
subj<-"Reading"
wf_title<-paste("Spring/Fall 2014 MAP RIT Scores (Percentiles) & Spring-to-Spring Goals\n",
                "KBCP", grade, subj, "by Spring/Fall Quartile\n",
                "(fall scores indicated by § preceeding name)")

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
    
    title<-paste("Spring/Fall 2014 MAP RIT Scores (Percentiles) & Spring-to-Spring Goals\n",
                    "KBCP", gr, subj,
                    "\n(fall scores indicated by § preceeding name)")
    
    p[[obj_name]] <- haid_plot(map_mv,
                               Grade==gr, 
                               MeasurementScale==subj, 
                               Year2==2014, 
                               p_title = title,
                               p_name_size=2
    )
  }
}

today<-today() %>% format(.,"%y%m%d")
cairo_pdf(file=paste0("graphs/KBCP_Waterfalls_Fall_", today, ".pdf"), 
          height = 8.25, 
          width=10.75, 
          onefile = TRUE)

p

dev.off()


