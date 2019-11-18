library(silounloadr)
library(mapvizieR)
library(dplyr)
library(purrr)
library(xlsx)
library(janitor)
library(stringr)


# Function to separte combined cdf to roster and results
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

subjs <- c("Reading", "Mathematics", "General Science")

nwea <- get_nwea(table_name = "MAP$comprehensive#plus_cps")

nwea <- collect(nwea)

nwea<-nwea %>% select(-ClassName, -TeacherName) %>% distinct()


nwea <- nwea %>%
  #filter(grepl("Create", SchoolName)) %>%
  mutate(Grade=as.integer(ifelse(Grade=="K", 0, Grade)),
         MeasurementScale=ifelse(MeasurementScale=="Science - General Science",
                                 "General Science",
                                 MeasurementScale)
  )

# get students





cdf <- nwea %>%
  filter(TermName == "Winter 2016-2017",
         str_detect(SchoolName, "Bloom")) %>%
  separate_cdf("KIPP Chicago")


map_mv<-mapvizieR(cdf = cdf$cdf, roster = cdf$roster)


current_ps_roster <- get_powerschool("students") %>% collect()



current_stus_bloom<-current_ps_roster %>% filter(schoolid == 400163, enroll_status == 0) %>%
  select(studentid=student_number, last_name, first_name, grade_level, home_room)

# reudce growth to jsut "start_only terms"
growth_df <- map_mv$growth_df %>%
  filter(start_fallwinterspring == "Winter",
         end_fallwinterspring == "Spring") %>%
  inner_join(current_stus_bloom, by = c("studentid")) %>%
  ungroup() %>%
  mutate(typical_target = start_testritscore + reported_growth,
         college_ready_target = start_testritscore + accel_growth) %>%
  select(measurementscale,
         #subject = measurementscale,
         grade_level,
         home_room,
         studentid,
         first_name,
         last_name,
         RIT = start_testritscore,
         typical_growth = reported_growth,
         typical_target,
         college_ready_growth = accel_growth,
         college_ready_target
         ) %>%
  arrange(measurementscale, grade_level, home_room, last_name, first_name)


cdf_2 <- nwea %>%
  filter(TermName %in% c("Spring 2015-2016", "Fall 2016_2017"),
         str_detect(SchoolName, "Bloom")) %>%
  separate_cdf("KIPP Chicago")


map_mv_2<-mapvizieR(cdf = cdf_2$cdf, roster = cdf_2$roster)


stus_with_s_f_scores <- map_mv_2$growth_df %>% ungroup() %>%
  filter(growth_window %in% c("Spring to Spring", "Fall to Spring")) %>%
  select(studentid, measurementscale) %>%
  distinct() %>%
  glimpse


growth_df_final <- growth_df %>%
  anti_join(stus_with_s_f_scores, by = c("studentid", "measurementscale")) %>%
  rename(subject = measurementscale) %>%
  arrange(subject, grade_level, home_room, last_name, first_name)


todays_date<-format(lubridate::today(), "%y%m%d")

write.csv(growth_df_final, file=paste0("reports/Bloom_winter_2_spring_goals_",todays_date,".csv"), na="")

