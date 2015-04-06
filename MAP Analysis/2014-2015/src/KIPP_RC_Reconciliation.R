setwd("~/Dropbox (KIPP Chicago Schools)/Data Analysis/MAP Analysis/2014-2015")

source("data/map_all_silo.R")

require(mapvizieR)

map_all_silo <- map_all_silo %>%
  mutate(Grade=as.integer(ifelse(Grade=="K", "0", Grade)))

data(ex_CombinedStudentsBySchool)
data(ex_CombinedAssessmentResults)

sel_names<-names(ex_CombinedStudentsBySchool)

sel_cdf_names <- names(ex_CombinedAssessmentResults)
# Drop DisctrictName
sel_names <- sel_names[sel_names!="DistrictName"]

map_silo <- map_all_silo %>% filter(TermName %in% c("Spring 2013-2014",
                                                    "Fall 2013-2014"),
                                    MeasurementScale %in% c("Reading",
                                                            "Mathematics"),
                                   # !is.na(TestID),
                                    GrowthMeasureYN=="TRUE",
                                   Tested_at_KIPP=="TRUE"
) %>% mutate(StudentID=as.character(StudentID),
             GrowthMeasureYN=as.logical(GrowthMeasureYN),
             TestDurationMinutes=as.integer(TestDurationMinutes),
             TestRITScore = as.integer(TestRITScore),
             TestPercentile = as.integer(TestPercentile),
             PercentCorrect = as.integer(PercentCorrect),
             TestID = as.integer(TestID)
)

map_students <- map_silo[, c(sel_names, "MeasurementScale", "TestPercentile")] %>%
  filter(MeasurementScale %in% c("Reading","Mathematics")) %>%
  mutate(school=abbrev(SchoolName, exceptions = list(old=c("KAPS", "KAMS"),
                                                     new=c("KACP", "KACP")
  )
  )
  ) %>%
  unique %>%
  as.data.frame

current_students<-map_students %>% 
  dplyr::filter(TermName=="Winter 2014-2015") %>%
  dplyr::select(studentid=StudentID, school, Grade) %>%
  unique




map_cdf <- map_silo %>%
  select(-StudentLastName:-Grade) %>%
  as.data.frame

map_mv<-mapvizieR(cdf = map_cdf, 
                  roster = map_students,
                  include_unsanctioned_windows=FALSE
)




map_mv$cdf %>% 
  filter(fallwinterspring=="Spring", 
         map_year_academic=="2013") %>% 
  group_by( measurementscale, grade, schoolname) %>% 
  summarize(N=n(), 
            N_3Q=sum(testquartile==3),
            N_4Q=sum(testquartile==4),
            pct_3Q=round(N_3Q/N*100), 
            pct_4Q=round(N_4Q/N*100)) %>% 
  arrange(desc(measurementscale), schoolname, grade) %>% 
  filter(grepl("Ascend", schoolname),  grade==0)

# another way

kcs_map<-map_all_silo %>% 
  filter(Grade==0, 
         MeasurementScale %in% c("Reading", "Mathematics"), 
         TermName=="Spring 2013-2014",
         Tested_at_KIPP=="TRUE") %>%
  mutate(TestQuartile = kipp_quartile(TestPercentile, return_factor = F))

