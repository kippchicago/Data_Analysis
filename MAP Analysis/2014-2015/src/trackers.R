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



# subjects
subjs <- c("Reading", "Mathematics", "General Science")

# Change grade "K" to 0 and recast Grade as integer.
map_all_silo<- map_all_silo %>% 
  mutate(Grade=as.integer(ifelse(Grade=="K", 0, Grade)),
         MeasurementScale=ifelse(MeasurementScale=="Science - General Science", 
                                 "General Science",
                                 MeasurementScale)
         ) 


# Get spring
map_spring <- map_all_silo %>% 
  filter(TermName=="Spring 2013-2014",
         MeasurementScale %in% subjs) %>%
  mutate(Grade2=Grade+1,
         Spring_Score=TestRITScore) %>%
  select(StudentID, 
         MeasurementScale,
         TestRITScore,
         Spring_Score)


# Get fall
map_fall <- map_all_silo %>% 
  filter(TermName=="Fall 2014-2015",
         MeasurementScale %in% subjs) %>%
  mutate(Fall_Score=TestRITScore) %>%
  select(-TestRITScore)


# Join Fall adnnd Spring


# Check if spring is na.  If so updated Fall_as_Spring indicator.  If Fall_as_Spring==T then update TestRITScore to Fall Score



map_joined <- left_join(map_fall, map_spring, by=c("StudentID", "MeasurementScale")) %>%
  mutate(Fall_as_Spring=is.na(Spring_Score),
         Spring_Score=ifelse(Fall_as_Spring==TRUE, Fall_Score, Spring_Score),
         TestRITScore=ifelse(Fall_as_Spring==TRUE, Fall_Score, TestRITScore)
         )


# Calcualte Growth

map_growth<-mapvizier(map_joined)$mapData  %>%
  mutate(
    Goal = ifelse(Grade==0, R42, R22),
    TypicalGrowth_Spring=Spring_Score+Goal,
    TieredGrowth_Spring=Spring_Score+round(Goal*KIPPTieredGrowth),
    TypicalGrowth_Winter=Spring_Score+round(Goal/2),
    TieredGrowth_Winter=Spring_Score+round(Goal*KIPPTieredGrowth/2),
    SchoolInitials=ifelse(SchoolInitials=="KAPS", "KAP", SchoolInitials)
  ) %>% 
  select(SchoolInitials, 
         MeasurementScale,
         Grade, 
         StudentFirstName, 
         StudentLastName, 
         Spring_Score,
         Fall_Score,
         TypicalGrowth_Winter,
         TieredGrowth_Winter,
         TypicalGrowth_Spring,
         TieredGrowth_Spring,
         Fall_as_Spring,
         Tested_at_KIPP
         
  ) %>%
  arrange(SchoolInitials, MeasurementScale, Grade, StudentLastName, StudentFirstName)


todays_date<-format(today(), "%y%m%d")
write.csv(map_growth, file=paste0("reports/Tracker_",todays_date,".csv"), na="")



# Excel manipulation ####
#schools <- "KAP"
schools<-map_growth$SchoolInitials %>% unique
for(school in schools) {
  
  file_in <- sprintf("Trackers/templates/MAP Tracker - SY15 %s.xlsx", school)
  xls_wb<-loadWorkbook(file=file_in)
  sheets<-getSheets(xls_wb)
  
  map_school<-
    map_growth %>%
    filter(SchoolInitials==school)
  
  for (subj in unique(map_school$MeasurementScale)){
    map_subj<-map_school %>% filter(MeasurementScale==subj)
    for(gr in unique(map_subj$Grade)){
      
      map_out <- map_subj %>%
        filter(Grade==gr)
      
      
      
      subj<-ifelse(subj=="General Science", "Science", subj)
      sheet_name <- paste(school, gr, subj)
      
      message(paste("Getting sheet:", sheet_name))
      sheet<-sheets[[sheet_name]]  
      
      message("Adding name, spring and fall scores and winter goals")
      addDataFrame(map_out %>%
                     select(StudentFirstName, 
                            StudentLastName,
                            Spring_Score,
                            Fall_Score,
                            TypicalGrowth_Winter,
                            TieredGrowth_Winter
                     ),
                   sheet=sheet,
                   col.names=FALSE,
                   row.names=FALSE,
                   startRow=4,
                   startColumn=1
      )
      
      
      message("Adding spring goals")
      addDataFrame(map_out %>%
                     select(
                       TypicalGrowth_Spring,
                       TieredGrowth_Spring
                     ),
                   sheet=sheet,
                   col.names=FALSE,
                   row.names=FALSE,
                   startRow=4,
                   startColumn=10
      )
      
      
      message("Adding score source data")
      addDataFrame(map_out %>%
                     select(Fall_as_Spring, 
                            Tested_at_KIPP
                     ),
                   sheet=sheet,
                   col.names=FALSE,
                   row.names=FALSE,
                   startRow=4,
                   startColumn=15
      )
      
      
      
    }
  }
  message("Saving file.")
  file_out <- sprintf("Trackers/complete/MAP Tracker - SY15 %s.xlsx", school)
  saveWorkbook(xls_wb, file=file_out)
  
}



