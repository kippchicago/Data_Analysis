library(silounloadr)
library(tidyverse)


terms <- c("Spring 2016-2017", "Spring 2017-2018", "Spring 2018-2019")

get_by_terms <- function(term){
  get_nwea_map(table_name = "cdf_combined_kipp_cps") %>%
    filter(term_name == term) %>%
    collect()
}

map <- 
  purrr::map_df(terms,
                ~ get_by_terms(.x)
  )

map_middle <- map %>%
  filter(school_name %in% c("KIPP Create College Prep", "KIPP Bloom College Prep", "KIPP Ascend Middle School",
                            "KIPP One Academy", "KIPP Academy Chicago"))

students_table <- get_powerschool("students") %>%
  select(student_number,
         schoolid,
         dob,
         ethnicity,
         gender,
         grade_level, 
         entrydate,
         exitdate) %>% 
  collect()

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
  filter(schoolabbr %in% c("KAMS", "KACP", "KAC", "KBCP", "KOA")) %>%
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
  group_by(schoolabbr, grade_level, student_number, first_name, last_name, school_year) %>%
  summarize(enrolled = sum(enrolled),
            present = sum(present),
            absent = sum(absent),
            tardy = sum(tardy)) 



# cc for courses

cc <- get_powerschool("cc") %>%
  select(course_number,
         studentid,
         teacherid, 
         termid,
         schoolid) %>%
  filter(termid >= 2700,
         schoolid %in% c(7810, 400146, 400163, 400180)) %>%
  collect()





