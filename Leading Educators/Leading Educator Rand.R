library(silounloadr)
library(tidyverse)
library(lubridate)

schools <- data_frame(schoolid = c(78102, 7810, 400146, 4001462, 400163, 4001802, 400180, 4001632),
                      schoolname = c("Ascend Primary", "Ascend Middle", "Academy Chicago","Academy Chicago Primary", "Bloom", "One Primary", "One Academy", "Bloom Primary"),
                      schoolabbr =c("KAP", "KAMS", "KAC", "KACP", "KBCP", "KOP", "KOA", "KBP"))

terms <- c("Spring 2016-2017", "Spring 2017-2018", "Spring 2018-2019")

get_by_terms <- function(term){
  get_nwea_map(table_name = "cdf_combined_kipp_cps") %>%
    select(student_id,
           student_first_name,
           student_last_name,
           student_ethnic_group,
           term_name,
           school_name,
           measurement_scale,
           test_start_date,
           test_ritscore) %>%
    filter(term_name == term,
           measurement_scale %in% c("Reading", "Mathematics")) %>%
    collect()
}

map <- 
  purrr::map_df(terms,
                ~ get_by_terms(.x)
  )

map_all <- map %>%
  filter(school_name %in% c("KIPP Create College Prep", "KIPP Bloom College Prep", "KIPP Ascend Middle School",
                            "KIPP One Academy", "KIPP Academy Chicago", "KIPP One Primary", "KIPP Ascend Primary", "KIPP Bloom Primary", "KIPP Ascend Primary School"))

map_year <- function(term_name) {
  map_all %>%
    filter(term_name == term_name) %>%
    select(-c(student_first_name,
            #  student_ethnic_group
              student_last_name)) %>%
    rename(student_number = student_id) %>%
    group_by(student_number,
            # student_ethnic_group,
             term_name,
             school_name) %>%
    pivot_wider(names_from = measurement_scale, values_from = c(test_start_date, test_ritscore))
}

map_1617 <- map_year("Spring 2016-2017")
map_1718 <- map_year("Spring 2017-2018")
map_1819 <- map_year("Spring 2018-2019")



students <- get_powerschool("students") %>%
  select(student_number,
        # schoolid,
         dob,
         ethnicity,
         gender,
        # grade_level, 
         entrydate,
         exitdate,
         first_name,
         last_name,
         id) %>% 
  collect()

all_enroll <- get_powerschool("ps_enrollment_all") %>%
  select(id = studentid,
         schoolid,
         sy_entrydate = entrydate,
         sy_exitdate = exitdate,
         grade_level,
         yearid) %>%
  filter(yearid %in% c(26, 27, 28)) %>%
 # filter(grade_level > 3) %>%
  collect()

all_enroll_students <- all_enroll %>%
  left_join(students, by = "id") %>%
  select(-c(entrydate,
            exitdate))

#### Attendance ####

# get attendance
attendance <- get_powerschool("attendance") %>%
  filter(att_date >= lubridate::ymd("2016-08-20")) %>%
  filter(att_date <= lubridate::ymd("2019-06-23")) %>%
  filter(att_mode_code == "ATT_ModeDaily") %>%
  collect()

membership <- silounloadr::get_powerschool("ps_membership_reg") %>% 
  filter(yearid %in% c(26, 27, 28)) %>% 
  select(studentid,
         schoolid,
         date = calendardate,
         enrolled = studentmembership,
         grade_level,
         attendance = ATT_CalcCntPresentAbsent) %>%
  collect() 

# get attendance code table  
attendance_code <- get_powerschool("attendance_code") %>%
  mutate(att_code = if_else(att_code == "true", "T", att_code)) %>% #
  collect()

attendance_complete <- attendance %>%
  right_join(attendance_code %>% 
               select(attendance_codeid = id,
                      att_code),
             by = "attendance_codeid")

# combine membership with attendance complete table
member_att <- membership  %>%
  left_join(attendance_complete %>%
              select(studentid,
                     att_date,
                     att_code
                     #presence_status_cd
              ),
            by =c("studentid",
                  "date" = "att_date"))

# Identify whether each att_code is enrolled, present, absent, or tardy for each student for each day
attend_student <- member_att %>%
  filter(date >= lubridate::ymd("2016-08-20")) %>%
  filter(date <= lubridate::ymd("2019-06-23")) %>%
  mutate(enrolled0 = 1,
         enrolled = if_else(att_code == "D" & !is.na(att_code), 0, enrolled0),
         present0 = ifelse(is.na(att_code) | att_code == "", 1, 0),
         present1 = ifelse(att_code %in%  c("A", "S"), 0, present0),
         present2 = ifelse(att_code == "H", 0.5, present1),
         present3 = ifelse(att_code %in% c("T", "E", "L", "I"), 1, present2),
         present = ifelse(is.na(present2), 1, present3),
         absent = (1 - present)*enrolled,
         tardy = ifelse(att_code %in% "T", 1, 0)) %>%
  left_join(students %>%
              select(studentid = id,
                     student_number,
                     first_name,
                     last_name),
            by="studentid") %>%
  inner_join(schools, by=c("schoolid")) %>%
  filter(schoolabbr %in% c("KAMS", "KAC", "KBCP", "KOA", "KOP", "KBP", "KAP")) %>%
  mutate(school_year = case_when(
    ymd(date) >= ymd("2016-08-20") & ymd(date) <= ymd("2017-06-23") ~ "2016-2017",
    ymd(date) >= ymd("2017-08-20") & ymd(date) <= ymd("2018-06-23") ~ "2017-2018",
    TRUE ~ "2018-2019"
  )) %>%
  select(studentid,
         student_number,
         first_name,
         last_name,
         grade_level,
         school_year,
         schoolid,
         schoolname,
         schoolabbr,
         date,
         att_code,
         enrolled,
         present,
         absent,
         tardy)


# summarize for every student separated by school year
attend_school_grade_student <- attend_student %>%
  group_by(#schoolabbr, 
           student_number, first_name, last_name, school_year) %>%
  summarize(enrolled = sum(enrolled),
            present = sum(present),
            absent = sum(absent),
            tardy = sum(tardy)) %>%
  ungroup() %>%
  select(student_number,
         school_year,
         enrolled,
         present,
         absent,
         tardy)


attend_school_grade_student_absent <- attend_school_grade_student %>%
  #ungroup() %>%
  select(student_number,
         school_year,
         absent) %>%
  # tardy) %>%
  group_by(student_number, school_year) %>% 
  spread(key = school_year, value = absent) %>%
  rename("absences_1617" = '2016-2017',
         "absences_1718" = '2017-2018',
         "absences_1819" = '2018-2019')

attend_school_grade_student_enrolled <- attend_school_grade_student %>%
  select(student_number,
         school_year,
         enrolled) %>%
  group_by(student_number, school_year) %>%
  spread(key = school_year, value = enrolled) %>%
  rename("enrolled_1617" = '2016-2017',
         "enrolled_1718" = '2017-2018',
         "enrolled_1819" = '2018-2019')

attend_student <- attend_school_grade_student_absent %>%
  left_join(attend_school_grade_student_enrolled, by = "student_number")


# 
# pivot_wider(id_cols = student_number, names_from = school_year, values_from = c(enrolled, absent))

# could ideally spread enrolled and Absences at once, didn't get it to work



#### Courses ####

cc <- get_powerschool("cc") %>%
  select(course_number,
         studentid,
         teacherid, 
         termid,
         schoolid) %>%
  filter(termid >= 2700,
         schoolid %in% c(7810, 400146, 400163, 400180)) %>%
  collect()

schoolstaff <- get_powerschool("schoolstaff") %>%
  select(dcid,
         id,
         users_dcid) %>% 
  collect()

cc_schoolstaff <- cc %>%
  left_join(schoolstaff %>%
              rename(teacherid = id), by = "teacherid")

users <- get_powerschool("users") %>%
  select(dcid,
         email_addr,
         first_name,
         last_name,
         homeschoolid,
         teachernumber) %>%
  collect()

cc_schoolstaff_users <- cc_schoolstaff %>%
  left_join(users %>%
              rename(users_dcid = dcid), by = "users_dcid") 

teachers_course <- cc_schoolstaff_users %>%
  group_by(teachernumber, course_number, termid, schoolid, email_addr, first_name, last_name, homeschoolid) %>%
  distinct(teachernumber) %>%
  filter(!grepl("att", course_number))

write_csv(teachers_course, "~/Downloads/teachers_course.csv")



#### Displinary - deans list suspensions ####

suspen_raw <- get_deanslist("suspensions") %>%
  select(suspension_id,
         student_number = student_school_id,
         student_first,
         student_last,
         school_name,
         actions,
         penalties,
         reported_details,
         admin_summary,
         category,
         grade_level_short,
         infraction,
         issue_ts) %>%
#  filter(issue_ts_date >= "2016-08-01 00:00") %>%
  collect(n = Inf) %>%
  janitor::clean_names("old_janitor")

issue_date_ts <- suspen_raw %>%
  pull(issue_ts) %>%
  map_df(jsonlite::fromJSON) %>%
  pull(date) %>%
  ymd_hms(tz = "America/Chicago")

suspen <- suspen_raw %>%
  mutate(date = issue_date_ts)

penalties <- suspen$penalties %>%
  purrr::map_df(~jsonlite::fromJSON(.x)) %>%
  janitor::clean_names("old_janitor") %>%
  select(suspensionid, 
         startdate, 
         enddate, 
         numdays,
         penaltyname 
  ) %>%
  mutate(startdate = ymd(startdate),
         enddate = ymd(enddate),
         diff_days = enddate - startdate,
         numdays = as.integer(numdays)) %>%
  arrange(startdate) %>%
  #filter(!is.na(startdate)) %>%
  mutate(suspensionid = as.integer(suspensionid))

oss <- suspen %>%
  inner_join(penalties %>% 
               filter(str_detect(penaltyname, "Out of School Suspension")),
             by = c("suspension_id" = "suspensionid")) 

suspen_oss <- oss %>%
  mutate(create_date = as_date(date)) %>%
  filter(create_date < '2019-08-01') %>%
  mutate(create_my = format(create_date, "%Y-%m")) %>%
   mutate(create_sy = case_when(
     create_my <= '2016-12' | create_my <= '2017-06' ~ "SY-2016-2017",
     create_my <= '2017-12' | create_my <= '2018-06' ~ "SY-2017-2018",
     create_my <= '2018-12' | create_my <= '2019-06' ~ "SY-2018-2019"
   )) %>%
  filter(!is.na(create_sy))

suspen_oss_count <- suspen_oss %>%
  group_by(#grade_level_short,
           student_number,
           create_sy) %>%
  summarize(suspensions_year = n()) %>%
  spread(key = "create_sy", value = "suspensions_year") %>%
  #replace(., is.na(.), "0") %>%
  rename("suspen_1617" = "SY-2016-2017",
         "suspen_1718" = "SY-2017-2018",
         "suspen_1819" = "SY-2018-2019")

# Race Codes

races <- data_frame(ethnicity = c(10, 4, 2, 5, 6, 9, 1),
                      ethnicity_term = c("American Indian", "Asian", "Black", "Latino/Hispanic", "Native Hawaiian or other Pacific Islander",
                                    "Two or more races", "White"))
                    



#### Final Tables ####


# 16-17 SY with attendance, suspensions, and demographic info

all_1617 <- attend_student %>%
  select(-c(absences_1718,
            absences_1819,
            enrolled_1718,
            enrolled_1819)) %>%
  left_join(suspen_oss_count %>%
              select(-c(suspen_1718,
                        suspen_1819)),
                by = "student_number") %>%
  left_join(all_enroll_students,
                by = "student_number") %>%
  filter(yearid == 26,
         !is.na(enrolled_1617)) %>%
  left_join(map_1617, by = "student_number") %>%
  left_join(races, by = "ethnicity")

all_1617$suspen_1617[is.na(all_1617$suspen_1617)] <- 0

all_1617 <- all_1617 %>%
  select(student_number,
         schoolid,
         school_name,
         dob,
         ethnicity_term,
       #  student_ethnic_group,
         gender,
         grade_level,
         enrolled_1617,
         absences_1617,
         suspen_1617,
         test_start_date_Mathematics,
         test_ritscore_Mathematics,
         test_start_date_Reading,
         test_ritscore_Reading,
         sy_entrydate,
         sy_exitdate)


# 17-18 SY with attendance, suspensions, and demographic info

all_1718 <- attend_student %>%
  select(-c(absences_1617,
            absences_1819,
            enrolled_1617,
            enrolled_1819)) %>%
  left_join(suspen_oss_count %>%
              select(-c(suspen_1617,
                        suspen_1819)),
            by = "student_number") %>%
  left_join(all_enroll_students,
            by = "student_number") %>%
  filter(yearid == 27,
         !is.na(enrolled_1718)) %>%
  left_join(map_1718, by = "student_number") %>%
  left_join(races, by = "ethnicity")

all_1718$suspen_1718[is.na(all_1718$suspen_1718)] <- 0

all_1718 <- all_1718 %>%
  select(student_number,
         schoolid,
         school_name,
         dob,
         ethnicity_term,
     #    student_ethnic_group,
         gender,
         grade_level,
         enrolled_1718,
         absences_1718,
         suspen_1718,
         test_start_date_Mathematics,
         test_ritscore_Mathematics,
         test_start_date_Reading,
         test_ritscore_Reading,
         sy_entrydate,
         sy_exitdate)


# 18-19 SY with attendance, suspensions, and demographic info

all_1819 <- attend_student %>%
  select(-c(absences_1617,
            absences_1718,
            enrolled_1617,
            enrolled_1718)) %>%
  left_join(suspen_oss_count %>%
              select(-c(suspen_1617,
                        suspen_1718)),
            by = "student_number") %>%
  left_join(all_enroll_students,
            by = "student_number") %>%
  filter(yearid == 28,
         !is.na(enrolled_1819)) %>%
  left_join(map_1819, by = "student_number") %>%
  left_join(races, by = "ethnicity")

all_1819$suspen_1819[is.na(all_1819$suspen_1819)] <- 0

all_1819 <- all_1819 %>%
  select(student_number,
         schoolid,
         school_name,
         dob,
         ethnicity_term,
       #  student_ethnic_group,
         gender,
         grade_level,
         enrolled_1819,
         absences_1819,
         suspen_1819,
         test_start_date_Mathematics,
         test_ritscore_Mathematics,
         test_start_date_Reading,
         test_ritscore_Reading,
         sy_entrydate,
         sy_exitdate)

write_csv(all_1617, "~/Downloads/RAND_Students_SY16-17.csv")
write_csv(all_1718, "~/Downloads/RAND_Students_SY17-18.csv")
write_csv(all_1819, "~/Downloads/RAND_Students_SY18-19.csv")


