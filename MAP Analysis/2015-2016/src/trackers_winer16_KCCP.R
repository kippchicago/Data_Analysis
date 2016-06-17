require(ProjectTemplate)
load.project()
require(purrr)


subjs <- c("Reading", "Mathematics", "General Science")

map_create<- map_all_silo %>%
  filter(grepl("Create", SchoolName)) %>%
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


current_stus_create<-current_ps_roster %>% filter(SchoolID == 400146) %>%
  select(StudentID, StudentLastName, StudentFirstName,Current_Grade = Grade_Level, Home_Room)

map_spring <- map_create %>%
  filter(TermName=="Spring 2014-2015",
         MeasurementScale %in% subjs) %>%
  inner_join(current_stus_create, by = "StudentID") %>%
  mutate(
    Orignal_Score=TestRITScore,
    Original_Percentile=TestPercentile,
    Original_Term = "Spring 2015",
    Fall_as_Spring = FALSE)

# find current students with missing spring scores ###


current_stus_no_spring <- map_spring %>%
  split(.$MeasurementScale) %>%
  map(~anti_join(current_stus_bloom,
                 .,
                 by=c("StudentID")
  ) %>%
    mutate(MeasurementScale = unique(.x$MeasurementScale)) %>%
    select(StudentID, MeasurementScale) %>%
    unique) %>%
  bind_rows

# Get fall
map_fall_no_spring <- map_create %>%
  filter(TermName=="Fall 2015-2016",
         MeasurementScale %in% subjs) %>%
  inner_join(current_stus_no_spring,
             by = c("StudentID", "MeasurementScale")) %>%
  mutate(Current_Grade=Grade,
         Orignal_Score=TestRITScore,
         Original_Percentile=TestPercentile,
         Original_Term = "Fall 2015",
         Fall_as_Spring = TRUE,
         TermName = "Spring 2014-2015")

# impute misssing scores
map_fall_no_spring_reading <-  map_fall_no_spring %>%
  filter(MeasurementScale == "Reading") %>%
  mutate(TestRITScore = as.integer(sqrpr::cps_equate(TestRITScore, MeasurementScale, Grade)),
         CPS_Imputed = TRUE,
         Grade = Grade -1)

map_fall_no_spring_math <-  map_fall_no_spring %>%
  filter(MeasurementScale == "Mathematics") %>%
  mutate(TestRITScore = as.integer(sqrpr::cps_equate(TestRITScore, MeasurementScale, Grade)),
         CPS_Imputed = TRUE,
         Grade = Grade -1)

map_fall_no_spring_sci <-  map_fall_no_spring %>%
  filter(MeasurementScale == "General Science") %>%
  mutate(CPS_Imputed = FALSE,
         Grade = Grade -1)

map_joined <- dplyr::rbind_list(map_spring %>%
                                  mutate(CPS_Imputed = FALSE),
                                map_fall_no_spring_reading,
                                map_fall_no_spring_math,
                                map_fall_no_spring_sci
) %>%
  group_by(StudentID, TermName) %>%
  filter(Grade == min(Grade)) %>% #need to remove retained students
  ungroup

# Calcualte Growth

cdf <- separate_cdf(map_joined, "KIPP Chicago")


map_create_mv<-mapvizieR(cdf = cdf$cdf, roster = cdf$roster)

create_fall <- map_create %>%
  filter(TermName == "Fall 2015-2016") %>%
  select(StudentID, Subject = MeasurementScale, Fall_RIT = TestRITScore)

create_winter <- map_create %>%
  filter(TermName == "Winter 2015-2016") %>%
  select(StudentID, Subject = MeasurementScale, Winter_RIT = TestRITScore)

create_goals <-map_create_mv$growth_df %>%
  filter(growth_window == "Spring to Spring",
         match_status == "only start",
         start_growthmeasureyn) %>%
  mutate(Spring_to_Spring_Goal = start_testritscore + reported_growth,
         Spring_to_Spring_CR_Goal = start_testritscore + accel_growth) %>%
  select(StudentID = studentid,
         Subject = measurementscale,
         Last_Spring_RIT = start_testritscore,
         Spring_to_Spring_Goal,
         Spring_to_Spring_CR_Goal
         )

out <-  create_winter %>%
  left_join(create_fall, by = c("StudentID", "Subject"))  %>%
  left_join(create_goals, by = c("StudentID", "Subject")) %>%
  inner_join(current_stus_create, by="StudentID") %>%
  left_join(map_joined %>%
               select(StudentID, Subject = MeasurementScale, CPS_Imputed),
             by = c("Subject", "StudentID")) %>%
  mutate(Last_Spring_RIT = ifelse(Subject == "General Science",
                                  NA,
                                  Last_Spring_RIT)) %>%
  select(Subject,
         StudentID,
         StudentLastName,
         StudentFirstName,
         Current_Grade,
         Home_Room,
         Last_Spring_RIT,
         Fall_RIT,
         Winter_RIT,
         Spring_to_Spring_Goal,
         Spring_to_Spring_CR_Goal,
         Spring_Estimated_from_Fall = CPS_Imputed
         ) %>%
  arrange(Subject, Current_Grade, Home_Room, StudentLastName, StudentFirstName)

readr::write_csv(out, path="reports/KCCP_FWS_MAP_160429.csv")

