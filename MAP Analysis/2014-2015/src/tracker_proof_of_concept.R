# Traccker proof of concept 
# Getting Spring/Fall Data for KCCP 6 Reading and KAP 3 Math

# This one has a wrinkle.

# We need to get spring scores for those that have it, and make them look like spring scores

# So we need to identify which kids have only fall scores from this sesssion and no spring 
# scrores from Spirng 2013-2014).  Select only them from Fall 2014-15, 
#change the term to Spring 2013-2014, and then bind them to the actual Spring 2013-2014.
# hopefully run mapvizier on that one season and feed it to haid_plot. Fingers crossed.

setwd("~/Dropbox (KIPP Chicago Schools)/Data Analysis/MAP Analysis/2014-2015")
require(ProjectTemplate)
load.project()


#map_mv <- mapvizier(map_all)

grade_kccp <- 6
grade_kap <- 3

# Get KAP 3 Math and KCCP 6 Reading for last spring 
# and create Grade2 for incremented grade


map_spring <- map_all %>% 
  filter(TermName=="Spring 2013-2014") %>%
  filter(
         (SchoolName=="KIPP Ascend Primary School" & Grade==grade_kap-1 & MeasurementScale=="Mathematics") |
            (SchoolName=="KIPP Create College Prep" & Grade==grade_kccp-1 & MeasurementScale=="Reading") 
             ) %>%
  mutate(Grade2=Grade+1)

map_spring_2<-mapvizier(map_spring)$mapData  %>%
  mutate(Spring_Score=TestRITScore,
         TypicalGrowth_Spring=Spring_Score+R22,
         TieredGrowth_Spring=Spring_Score+round(R22*KIPPTieredGrowth),
         TypicalGrowth_Winter=Spring_Score+round(R22/2),
         TieredGrowth_Winter=Spring_Score+round(R22*KIPPTieredGrowth/2)
         ) %>%
  select(StudentID, 
         MeasurementScale, 
         Spring_Score, 
         TypicalGrowth_Spring,
         TieredGrowth_Spring,
         TypicalGrowth_Winter,
         TieredGrowth_Winter)


# Get Get both for Fall, return only those missing test scores, change to 
# and change term name to Spring and add a start to last name to indicate we will use fall score
map_fall <- map_all %>% 
  filter(TermName=="Fall 2014-2015") %>%
  filter(
    (SchoolName=="KIPP Ascend Primary School" & Grade==grade_kap & MeasurementScale=="Mathematics") |
      (SchoolName=="KIPP Create College Prep" & Grade==grade_kccp & MeasurementScale=="Reading") 
  ) %>%
  mutate(Fall_Score=TestRITScore)

map_out<- left_join(map_fall, map_spring_2, by=c("StudentID", "MeasurementScale")) %>%
  select(SchoolName, 
         MeasurementScale,
         Grade, 
         StudentFirstname, 
         StudentLastname, 
         Spring_Score,
         Fall_Score,
         TypicalGrowth_Winter,
         TieredGrowth_Winter,
         TypicalGrowth_Spring,
         TieredGrowth_Spring
         ) %>%
  arrange(SchoolName, MeasurementScale, StudentLastname, StudentFirstname)
write.csv(map_out, file="reports/Tracker_PoC_141007.csv", na="")
