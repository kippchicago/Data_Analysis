# Traccker
# Getting Spring/Fall Data for KCCP 6 Reading and KAP 3 Math

# This one has a wrinkle.

# We need to get spring scores for those that have it, and make them look like spring scores

# So we need to identify which kids have only fall scores from this sesssion and no spring
# scrores from Spirng 2013-2014).  Select only them from Fall 2014-15,
#change the term to Spring 2013-2014, and then bind them to the actual Spring 2013-2014.
# hopefully run mapvizier on that one season and feed it to haid_plot. Fingers crossed.
# Finally we will estimate spring map for anyone who doen't have one for KAP 5th grade
setwd("~/Dropbox (KIPP Chicago Schools)/Data Analysis/MAP Analysis/2015-2016")
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


separate_cdf <- function(combinded_cdf, district_name = "Not provided"){
  ar_names <- names(ex_CombinedAssessmentResults)
  stu_names <- names(ex_CombinedStudentsBySchool)

  if (!"districtname" %in% tolower(names(combinded_cdf))) {
    combinded_cdf <- combinded_cdf %>% mutate_(DistrictName = ~district_name)
  }

  roster<-combinded_cdf %>%
    select_(.dots = stu_names) %>%
    unique

  cdf<-combinded_cdf %>% select(-StudentLastName:-StudentFirstName,
                               -StudentMI:-StudentGender,
                               -Grade) %>%
    mutate(TestID=as.character(TestID))

  out <- list(cdf = cdf,
              roster = roster)

}


#map_mv <- mapvizieR(cdf, roster)

# get current non-kinder
current_stus_non_k<-current_ps_roster %>% filter(Grade_Level != 0) %>%
  select(StudentID)

# Get spring
map_spring <- map_all_silo %>%
  filter(TermName=="Spring 2014-2015",
         MeasurementScale %in% subjs,
         StudentID %in% current_stus_non_k$StudentID) %>%
  mutate(Current_Grade=Grade+1,
         Orignal_Score=TestRITScore,
         Original_Percentile=TestPercentile,
         Original_Term = "Spring 2015",
         Fall_as_Spring = FALSE)

# find current students with missing spring scores ###

current_stus_no_spring<-anti_join(current_ps_roster,
                                  map_spring,
                                  by=c("StudentID")
                                  ) %>%
  select(StudentID) %>%
  unique


# Get fall
map_fall <- map_all_silo %>%
  filter(TermName=="Fall 2015-2016",
         Grade != 0,
         MeasurementScale %in% subjs,
         StudentID %in% current_stus_no_spring$StudentID) %>%
  mutate(Current_Grade=Grade,
         Orignal_Score=TestRITScore,
         Original_Percentile=TestPercentile,
         Original_Term = "Fall 2015",
         Fall_as_Spring = TRUE,
         TermName = "Spring 2014-2015")

# get fall kinder
current_stus_kinder<-current_ps_roster %>% filter(Grade_Level == 0) %>% select(StudentID)

map_fall_kinder <- map_all_silo %>%
  filter(TermName=="Fall 2015-2016",
         MeasurementScale %in% subjs,
         StudentID %in% current_stus_kinder$StudentID) %>%
  mutate(Current_Grade=Grade,
         Orignal_Score=TestRITScore,
         Original_Percentile=TestPercentile,
         Original_Term = "Fall 2015",
         Fall_as_Spring = NA,
         CPS_Imputed = NA)



# Check if spring is na.  If so updated Fall_as_Spring indicator.
#If Fall_as_Spring==T then update TestRITScore to Fall Score
# NEW if KAP 5 Then updated Fall_as_Spring to FALSE, update
# Imputed_from_Fall to TRUE and filter those students, ipmute for
# math and reading, bind, then join

map_fall_reading <-  map_fall %>%
  filter(MeasurementScale == "Reading") %>%
  mutate(TestRITScore = as.integer(sqrpr::cps_equate(TestRITScore, MeasurementScale, Grade)),
         CPS_Imputed = TRUE)

map_fall_math <-  map_fall %>%
  filter(MeasurementScale == "Mathematics") %>%
  mutate(TestRITScore = as.integer(sqrpr::cps_equate(TestRITScore, MeasurementScale, Grade)),
         CPS_Imputed = TRUE)


map_fall_sci <-  map_fall %>%
  filter(MeasurementScale == "General Science") %>%
  mutate(CPS_Imputed = FALSE)



map_joined <- dplyr::rbind_list(map_spring %>%
                                  mutate(CPS_Imputed = FALSE),
                                map_fall_reading,
                                map_fall_math,
                                map_fall_sci
                               ) %>%
  inner_join(current_ps_roster %>% select(-StudentFirstName,
                                          -StudentLastName),
             by = "StudentID")



# Calcualte Growth

cdf <- separate_cdf(map_joined, "KIPP Chicago")
map_mv<-mapvizieR(cdf = cdf$cdf, roster = cdf$roster)

cdf_k <- separate_cdf(map_fall_kinder, "KIPP Chicago")
map_mv_k<-mapvizieR(cdf = cdf_k$cdf, roster = cdf_k$roster)


# reudce growth to jsut "start_only terms"
growth_df <- map_mv$growth_df %>%
  filter(match_status == "only start") %>%
  inner_join(map_mv$cdf %>% select(testid,
                                   current_grade:cps_imputed,
                                   tested_at_kipp),
             by=c("start_testid" = "testid")) %>%
  inner_join(map_mv$roster, by = c("studentid"))


growth_df_k <- map_mv_k$growth_df %>%
  filter(match_status == "only start",
         growth_window == "Fall to Spring") %>%
  inner_join(map_mv_k$cdf %>% select(testid,
                                   current_grade:cps_imputed,
                                   tested_at_kipp),
             by=c("start_testid" = "testid")) %>%
  inner_join(map_mv_k$roster, by = c("studentid"))


growth_df_joined <- current_ps_roster %>%
  left_join(rbind_list(growth_df, growth_df_k),
            by = c("StudentID" = "studentid"))


#update school names
school_names <- data.frame(SchoolID=c(78102, 7810, 400146, 400163),
                           SchoolName = c("KIPP Ascend Primary School",
                                          "KIPP Ascend Middle School",
                                          "KIPP Create College Prep",
                                          "KIPP Bloom College Prep")
)

growth_df_joined$SchoolName<-
  school_names$SchoolName[match(growth_df_joined$SchoolID,
                                school_names$SchoolID)]



map_growth_final<-growth_df_joined %>%
  mutate(
    #Goal = ifelse(Current_Grade==0, R42, R22),
    TypicalGrowth_Spring=start_testritscore + reported_growth,
    TieredGrowth_Spring= start_testritscore + accel_growth,
    TypicalGrowth_Winter=start_testritscore + round(reported_growth/2),
    TieredGrowth_Winter=start_testritscore + round(accel_growth/2),
    SchoolInitials = abbrev(SchoolName, exceptions = list(old ="KAPS", new="KAP")),
    #Fall_Score = ifelse(original_term == "Fall 2015", orignal_score, NA),
    Fall_Score = orignal_score,
    Fall_as_Spring = fall_as_spring) %>%
  select(SchoolInitials,
         MeasurementScale = measurementscale,
         Grade=Grade_Level,
         StudentFirstName,
         StudentLastName,
         Spring_Score = start_testritscore,
         Fall_Score,
         TypicalGrowth_Winter,
         TieredGrowth_Winter,
         TypicalGrowth_Spring,
         TieredGrowth_Spring,
         Fall_as_Spring,
         Tested_at_KIPP = tested_at_kipp,
         CPS_imputed = cps_imputed
         #,Spring_Imputed

  ) %>%
  mutate(Spring_Score = ifelse(Grade==0, NA, Spring_Score)) %>% # fix for kinder
  arrange(SchoolInitials, MeasurementScale, Grade, StudentLastName, StudentFirstName)


todays_date<-format(today(), "%y%m%d")
write.csv(map_growth_final, file=paste0("reports/Tracker_",todays_date,".csv"), na="")



# Excel manipulation ####
#schools <- c("KCCP", "KBCP", "KAP", "KAMS")
schools <- map_growth_final$SchoolInitials %>% unique
for(school in schools) {

  file_in <- sprintf("Trackers/templates/MAP Tracker - SY16 %s.xlsx", school)
  xls_wb<-loadWorkbook(file=file_in)
  sheets<-getSheets(xls_wb)

  map_school<-
    map_growth_final %>%
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
                            Tested_at_KIPP,
                            CPS_imputed
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
