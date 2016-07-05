# Initial analysis of KTC sales force data.

require(dplyr)
require(lubridate)
require(silounloadr)


# get alumni tables

contact <- get_alumni("contact") %>% collect
account <- get_alumni("account") %>% collect
enrollment_c <- get_alumni("enrollment__c")  %>% collect()
#contact_note_c <- get_alumni("contact_note__c") %>% collect
#college_persistence_c <- get_alumni("college_persistence__c") %>% collect


glimpse(contact)
glimpse(account)
glimpse(enrollment_c)
glimpse(contact_note_c)
glimpse(college_persistence_c)


adb_lookup <- function(name) {
  contact %>%
    filter(grepl(name, Full_Name__c)) %>%

    inner_join(enrollment_c, by = c("Id" = "Student__c")) %>%
    inner_join(account %>% select(School__c = Id, school_name = Name, Type),
               by = "School__c") %>%
    glimpse
}

#academic year functin

calc_academic_year <- function(x,
                               format = "long",
                               date_parser=lubridate::ymd) {

  date_parsed <- date_parser(x) %>% as.POSIXlt()
  out <- date_parsed$year + (date_parsed$mo >= 6) + 1899

  # return SY
  if(format == "long") out <- sprintf("SY %s-%s", out, out+1)

  if(format == "short") {
    out <- sprintf("%s-%s",
            stringr::str_extract(out, "\\d{2}$"),
            stringr::str_extract(out+1, "\\d{2}$"))
  }

  if(format == "first_year") out <- out
  if(format == "second_year") out <- out + 1


  out

}


adb_lookup("Kiara Hampton")
adb_lookup("Villegas-Mack")


#Let's looko at Roiale's class
class_2016 <- contact %>%
  filter(KIPP_HS_Class__c==2016) %>%
  inner_join(enrollment_c, by = c("Id" = "Student__c")) %>%
  inner_join(account %>% select(School__c = Id, school_name = Name, Type),
             by = "School__c") %>%
  mutate(start_date = ymd(Start_Date__c),
         node_grade = ifelse(StartingGrade__c <=8, 8, StartingGrade__c),
         sy = calc_academic_year(Start_Date__c)) %>%
  arrange(Id, node_grade) %>%
  select(Id, LastName, FirstName, KIPP_HS_Class__c, school_name, node_grade, start_date, Type, Status__c)


x <- class_2016 %>%
  dplyr::filter(Status__c!="Graduated",
                Type != "KIPP School") %>%
  arrange(start_date) %>%
  group_by(Id) %>%
  summarise(N_transitions = n(),
            last_transition = last(Status__c, order_by = start_date),
            last_school =last(school_name, order_by = start_date),
            last_school_type = last(Type, order_by = start_date),
            node_grade= max(node_grade)
            ) %>%
  mutate(in_college = (last_transition == "Matriculated" & node_grade==13 & !grepl("2 yr", last_school_type)),
         ideal_path = (N_transitions==2 & in_college))

ggplot(x, aes(x=N_transitions-2)) +
  geom_histogram() +
  facet_grid(in_college~ . ) +
  theme_bw()

x %>%
  ungroup() %>%
  summarize(N = n(),
            N_ideal = sum(ideal_path),
            Pct_ideal = N_ideal/N,
            Pct_in_college = sum(in_college)/N)
