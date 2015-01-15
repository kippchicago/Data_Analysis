# Traccker 
# Getting Spring/Fall Data for KCCP 6 Reading and KAP 3 Math

# This one has a wrinkle.

# We need to get spring scores for those that have it, and make them look like spring scores

# So we need to identify which kids have only fall scores from this sesssion and no spring 
# scrores from Spirng 2013-2014).  Select only them from Fall 2014-15, 
#change the term to Spring 2013-2014, and then bind them to the actual Spring 2013-2014.
# hopefully run mapvizier on that one season and feed it to haid_plot. Fingers crossed.
# Finally we will estimate spring map for anyone who doen't have one for KAP 5th grade
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
  mutate(Current_Grade=Grade+1,
         Spring_Score=TestRITScore,
         Spring_Percentile=TestPercentile) %>%
  select(StudentID, 
         MeasurementScale,
         TestRITScore,
         Spring_Score,
         Spring_Percentile)


# Get fall
map_fall <- map_all_silo %>% 
  filter(TermName=="Fall 2014-2015",
         MeasurementScale %in% subjs) %>%
  mutate(Fall_Score=TestRITScore,
         Fall_Percentile=TestPercentile) %>%
  select(-TestRITScore)

# Get Grade-Measurement Scale unique combinations to use to build out roster
# based on fall map
grade_subjects <- map_fall %>% 
  select(Grade, MeasurementScale) %>% 
  unique

# Expand current ps roster by grade subjects 

roster_expanded<-current_ps_roster %>% 
  inner_join(grade_subjects, by=c("Grade_Level" = "Grade"))

# match Fall test to expanded roster (this ensures that we maintain 
# records for students who arrived after fall testing ended)

map_fall_expanded<- 
  left_join(roster_expanded, 
            map_fall %>%
              select(-StudentFirstName,
                     -StudentLastName,
                     -Grade),
            by=c("StudentID", "MeasurementScale")) %>%
  mutate(Current_Grade=Grade_Level,
         Grade=ifelse(Current_Grade==0, 0, Current_Grade-1)
         ) %>%
  select(-Grade_Level)

# Join Fall (expanded) and Spring


# Check if spring is na.  If so updated Fall_as_Spring indicator.  
#If Fall_as_Spring==T then update TestRITScore to Fall Score
# NEW if KAP 5 Then updated Fall_as_Spring to FALSE, update
# Imputed_from_Fall to TRUE and filter those students, ipmute for 
# math and reading, bind, then join 


map_joined <- left_join(map_fall_expanded, map_spring, by=c("StudentID", "MeasurementScale")) %>%
  mutate(Fall_Missing=is.na(Fall_Score),
         Spring_Missing=is.na(Spring_Score),
         Fall_as_Spring=is.na(Spring_Score) & !Fall_Missing,
         Spring_Score=ifelse(Fall_as_Spring==TRUE, Fall_Score, Spring_Score),
         TestRITScore=ifelse(Fall_as_Spring==TRUE, Fall_Score, TestRITScore),
         TestPercentile=ifelse(Fall_as_Spring==TRUE, Fall_Percentile, Spring_Percentile),
         GrowthMeasureYN="TRUE",
         Tested_at_KIPP=ifelse(is.na(Tested_at_KIPP), FALSE, Tested_at_KIPP)
         )

#update school names
school_names <- data.frame(SchoolID=c(78102, 7810, 400146, 400163),
                           SchoolName = c("KIPP Ascend Primary School",
                                          "KIPP Ascend Middle School",
                                          "KIPP Create College Prep",
                                          "KIPP Bloom College Prep")
                           )

map_joined$SchoolName<-
  school_names$SchoolName[match(map_joined$SchoolID, 
                                school_names$SchoolID)]





map_joined_kap_5<-map_joined %>%
  filter(SchoolName=="KIPP Ascend Primary School",
         Grade==5, 
         MeasurementScale %in% c("Reading", "Mathematics"),
         Fall_as_Spring==TRUE) %>%
  mutate(Fall_as_Spring=FALSE,
         Spring_Imputed=TRUE) %>%
  select(StudentID, 
         MeasurementScale, 
         TestRITScore=TestRITScore,
         Goal1RitScore = Goal1RitScore,
         Goal2RitScore = Goal2RitScore,
         Goal3RitScore = Goal3RitScore,
         Goal4RitScore = Goal4RitScore,
         PercentCorrect=PercentCorrect,
         TestDurationMinutes=TestDurationMinutes,
         Grade)


map_kap_5_reading_imputed<-map_joined_kap_5 %>% 
  filter(MeasurementScale=="Reading") %>%
  impute_spring(subject="reading")

map_kap_5_math_imputed<-map_joined_kap_5 %>% 
  filter(MeasurementScale=="Mathematics") %>%
  impute_spring(subject="math")

map_kap_5_imputed<-rbind(map_kap_5_reading_imputed, 
                         map_kap_5_math_imputed) %>%
  select(StudentID, 
         MeasurementScale, 
         Imputed_Spring_RIT =predict_spring_RIT
         ) %>%
  mutate(Imputed_Spring_RIT = round(Imputed_Spring_RIT))


map_joined<-left_join(map_joined, 
                map_kap_5_imputed, 
                by=c("StudentID", "MeasurementScale")
                ) %>%
  mutate(Fall_as_Spring=ifelse(!is.na(Imputed_Spring_RIT), 
                               FALSE, 
                               Fall_as_Spring),
         Spring_Imputed=!is.na(Imputed_Spring_RIT),
         TestRITScore=ifelse(Spring_Imputed, Imputed_Spring_RIT, TestRITScore),
         Spring_Score=ifelse(Spring_Imputed, Imputed_Spring_RIT, Spring_Score)
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
         Grade=Current_Grade, 
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
         #,Spring_Imputed
         
  ) %>%
  arrange(SchoolInitials, MeasurementScale, Grade, StudentLastName, StudentFirstName)


todays_date<-format(today(), "%y%m%d")
write.csv(map_growth, file=paste0("reports/Tracker_",todays_date,".csv"), na="")



# Excel manipulation ####
schools <- "KBCP"
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
                            #,Spring_Imputed
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
  file_out <- sprintf("Trackers/complete/MAP Tracker - SY15 %s %s.xlsx", school, todays_date)
  saveWorkbook(xls_wb, file=file_out)
  
}

# 5th KAP Spring Imputations ###
map_joined_kap_5<-map_joined %>%
  filter(SchoolName=="KIPP Ascend Primary School",
         Current_Grade==5, 
         MeasurementScale %in% c("Reading", "Mathematics"),
         Fall_as_Spring==TRUE) %>%
  mutate(Fall_as_Spring=FALSE,
         Spring_Imputed=TRUE) %>%
  select(StudentID, 
         MeasurementScale, 
         TestRITScore=TestRITScore,
         Goal1RitScore,
         Goal2RitScore,
         Goal3RitScore,
         Goal4RitScore,
         PercentCorrect,
         TestDurationInMinutes=TestDurationMinutes,
         Grade=Current_Grade)


map_kap_5_reading_imputed<-map_joined_kap_5 %>% 
  filter(MeasurementScale=="Reading") %>%
  impute_spring(subject="reading")

map_kap_5_math_imputed<-map_joined_kap_5 %>% 
  filter(MeasurementScale=="Mathematics") %>%
  impute_spring(subject="math")

map_kap_5_imputed<-rbind(map_kap_5_reading_imputed, 
                         map_kap_5_math_imputed) %>%
  select(StudentID, 
         MeasurementScale, 
         Imputed_Spring_RIT =predict_spring_RIT
  ) %>%
  mutate(Imputed_Spring_RIT = round(Imputed_Spring_RIT))


map_joined_5<-left_join(map_joined, 
                      map_kap_5_imputed, 
                      by=c("StudentID", "MeasurementScale")
) %>%
  mutate(Fall_as_Spring=ifelse(!is.na(Imputed_Spring_RIT), 
                               FALSE, 
                               Fall_as_Spring),
         Spring_Imputed=!is.na(Imputed_Spring_RIT),
         TestRITScore=ifelse(Spring_Imputed, Imputed_Spring_RIT, TestRITScore),
         Spring_Score=ifelse(Spring_Imputed, Imputed_Spring_RIT, Spring_Score)
  )



# Calcualte Growth

map_growth_5<-mapvizier(map_joined_5)$mapData  %>%
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
         Grade=Current_Grade, 
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
         ,Spring_Imputed
         
  ) %>%
  arrange(SchoolInitials, MeasurementScale, Grade, StudentLastName, StudentFirstName) %>%
  filter(Grade==5, SchoolInitials=="KAP")





# Excel manipulation ####
schools <- "KAP"
#schools<-map_growth$SchoolInitials %>% unique
for(school in schools) {
  
  file_in <- sprintf("Trackers/templates/MAP Tracker - SY15 %s.xlsx", school)
  xls_wb<-loadWorkbook(file=file_in)
  sheets<-getSheets(xls_wb)
  
  map_school<-
    map_growth_5 %>%
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
                            ,Spring_Imputed
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
  file_out <- sprintf("Trackers/complete/MAP Tracker - SY15 %s 5th Only %s.xlsx", school, todays_date)
  saveWorkbook(xls_wb, file=file_out)
  
}

