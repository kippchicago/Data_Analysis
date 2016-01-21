# Amy's New Year PD:
# S-W this year versus last

require(ProjectTemplate)

load.project()




map_sum  <- map_viz %>%  summary() %>%
  filter(end_map_year_academic>=2014,
         growth_window %in% c("Spring to Winter", "Fall to Winter"),
         measurementscale %in% c("Reading", "Mathematics")) %>% ungroup %>%
  select(-cohort_year)

map_sum_x_school <- map_sum %>%
  filter(!grepl("Primary", end_schoolname),
         !is.na(end_schoolname)) %>% # remove primary until grades 2-5 have tested
  group_by(end_map_year_academic,
           growth_window,
           measurementscale,
           end_schoolname) %>%
           summarize(n_students = sum(n_students),
                      n_typical = sum(n_typical),
                      pct_typical = n_typical/n_students)


map_sum_region <- map_sum_x_school %>% summarize(n_students = sum(n_students),
                               n_typical = sum(n_typical),
                               pct_typical = n_typical/n_students)


map_sum_region %>% summarize(n_students = sum(n_students),
                             n_typical = sum(n_typical),
                             pct_typical = n_typical/n_students) %>%
  ungroup %>%
  arrange(growth_window,
          end_map_year_academic)




map_sum_x_school %>%
  ungroup %>%
  group_by(end_map_year_academic,
           growth_window,
           end_schoolname) %>%
  summarize(n_students = sum(n_students),
            n_typical = sum(n_typical),
            pct_typical = n_typical/n_students)


# 6th grade Sping and Winter ####


# all studnets who took the test
rit_6_schools<-map_viz$cdf %>%
  filter(tested_at_kipp == "TRUE") %>%
  group_by(map_year_academic,
           termname,
           measurementscale,
           fallwinterspring,
           grade,
           schoolname) %>%
  summarize(n_students = n(),
            mean_rit = mean(testritscore, na.rm = TRUE),
            median_rit = median(testritscore, na.rm = TRUE)) %>%
  mutate(cohort = map_year_academic + 1 +(12-grade)) %>%
  filter(termname %in% c("Spring 2014-2015",
                         "Fall 2015-2016",
                         "Winter 2015-2016"),
         cohort == 2016 + 12 - 6,
         measurementscale %in% c("Reading", "Mathematics")
         )



# Attendance data ####

silo<-as.data.frame(read.dcf('config//silo_dw.dcf'))

ps_db <- RSQLServer:::src_sqlserver(server = "54.172.11.151:1433",
                                     database = "PS_mirror",
                                     properties = list(user = "silo",
                                                       password = "silKIPP1"
                                     )
)


att <- tbl(ps_db, "attendance") %>%
  select(STUDENTID, date = ATT_DATE,
         PRESENCE_STATUS_CD, ATT_CODE)

enroll <- tbl(ps_db, "membership") %>%
  select(STUDENTID, SCHOOLID, date=CALENDARDATE, YEARID)

enroll_att <- left_join(enroll, att,
                        by = c("STUDENTID", "date")) %>%
  filter(YEARID >= 24) %>%
  collect()


require(lubridate)

enroll_att_2 <- enroll_att %>%
  mutate(enrolled = 1,
         absent1 = ifelse(ATT_CODE == "A", 1, 0),
         absent = ifelse(ATT_CODE == "H", .5, absent1),
         date = ymd_hms(date.x)
  )

int_1415 <- ymd("140815") %--% ymd("150101")
int_1516 <- ymd("150815") %--% ymd("160101")

enroll_att_2 %>%
  filter(date %within% int_1415) %>%
  summarize(min_date = min(date),
            max_date = max(date),
            membership = sum(enrolled, na.rm=TRUE),
            absent = sum(absent, na.rm = TRUE),
            ADA = 1 - absent/membership)


  enroll_att_2 %>%
    filter(date %within% int_1516) %>%
    summarize(min_date = min(date),
              max_date = max(date),
              membership = sum(enrolled, na.rm=TRUE),
              absent = sum(absent, na.rm = TRUE),
              ADA = 1 - absent/membership)
