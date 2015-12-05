require(sqrpr)
require(RJDBC)
require(dplyr)
require(tidyr)
require(ggplot2)
#  get MAP data ####

setwd("~/Dropbox (KIPP Chicago Schools)/Data Analysis/SQRP/")
silo <- as.data.frame(read.dcf('config//silo_dw.dcf'))


drvr <- JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver',
             '~/Dropbox (KIPP Chicago Schools)/JDBC Drivers/sqljdbc_4.0/enu//sqljdbc4.jar')

conn <- dbConnect(drvr,
                  databaseName = as.character(silo$dbname),
                  url = as.character(silo$url),
                  user = as.character(silo$user),
                  password = as.character(silo$password)
)


# get viewAllAssessments ####
qry <- "SELECT * FROM NWEA..MAP$comprehensive#cps_included WHERE GrowthMeasureYN='TRUE'"
map_all_silo <- dbGetQuery(conn, qry)

# get AccomodationAssignments ####
qry <- "SELECT * FROM NWEA..AccommodationAssignment"
accomodations<-dbGetQuery(conn, qry)

names(accomodations) <- tolower(names(accomodations))


map_all_silo<-map_all_silo %>%
  mutate(
  Grade=ifelse(Grade=="K", 0, as.integer(Grade)),
  MeasurementScale=ifelse(grepl("General", MeasurementScale),
                          "General Science",
                          MeasurementScale)
  ) %>%
  filter(MeasurementScale %in% c("Reading", "Mathematics"))

names(map_all_silo) <- tolower(names(map_all_silo))

# Accomodationrogram Assignments ####
sped<-read.csv("data/sped_S15.csv")

sped15 <- sped %>% mutate(sped = "TRUE")


# sped15<-sped %>% filter(termname=="Spring 2014-2015") %>%
#   group_by(studentid) %>%
#   summarize(N=n()) %>%
#   mutate(sped="TRUE", studentid=as.numeric(studentid)) %>% select(-N)


# Get 14-15 data an calculate growth, attainment, pct ME
map_s14s15<-map_all_silo %>%
  filter(termname %in% c("Spring 2013-2014", "Spring 2014-2015")) %>%
  left_join(sped15, by="studentid") %>%
  mutate(sped=ifelse(is.na(sped), "FALSE", sped))

map_f14<-map_all_silo %>%
  filter(termname %in% c("Fall 2014-2015")) %>%
  select(studentid, measurementscale, testritscore, grade)

map_s15<-map_all_silo %>%
  filter(termname %in% c("Spring 2014-2015"),
         tested_at_kipp=="TRUE")



#calculate growth
kcs_1415_growth<-school_growth_percentile(map_s14s15, fall_equate_scores = map_f14)

#calculate_attainment
kcs_1415_attain<-school_attainment_percentile(map_s15)


#calculate growth priority groups ####
#AA
kcs_1415_growth_aa<-priority_group(kcs_1415_growth,
                              group_column = "studentethnicgroup",
                              group_id = "Black or African American",
                              fall_equate_scores = map_f14
                              )

#IEP
kcs_1415_growth_dl<-priority_group(kcs_1415_growth,
                              group_column = "sped",
                              group_id = "TRUE",
                              fall_equate_scores = map_f14
)

# calculate %me growth by school ####
# to do: move this to sqrpr
kcs_1415_pct_me<-calc_pct_me(kcs_1415_growth)








# Get 13-14 data ####


sped14 <- accomodations %>%
  filter(termname == "Spring 2013-2014") %>%
  select(studentid) %>%
  group_by(studentid) %>%
  summarize(N_accom=n()) %>%
  mutate(sped = "TRUE",
         studentid = as.numeric(studentid))


map_f13s14<-map_all_silo %>%
  filter(termname %in% c("Fall 2013-2014", "Spring 2013-2014")) %>%
  left_join(sped14, by="studentid") %>%
  mutate(sped=ifelse(is.na(sped), "FALSE", sped))

map_f13<-map_all_silo %>%
  filter(termname %in% c("Fall 2013-2014")) %>%
  select(studentid, measurementscale, testritscore, grade)

map_s14<-map_all_silo %>%
  filter(termname %in% c("Spring 2013-2014"),
         tested_at_kipp == "TRUE")



#calculate growth
kcs_1314_growth<-school_growth_percentile(map_f13s14, fall_equate_scores = map_f13)

#calculate_attainment
kcs_1314_attain<-school_attainment_percentile(map_s14)


#calculate growth priority groups ####
#AA
kcs_1314_growth_aa<-priority_group(kcs_growth,
                              group_column = "studentethnicgroup",
                              group_id = "Black or African American",
                              fall_equate_scores = map_f13
)

#IEP
kcs_1314_growth_dl<-priority_group(kcs_growth,
                              group_column = "sped",
                              group_id = "TRUE",
                              fall_equate_scores = map_f13
)

# calculate %me growth by school ####
# to do: move this to sqrpr
kcs_1314_pct_me<-calc_pct_me(kcs_growth)





# School level differences ####
# Set school years
kcs_1415_growth$school_level$sy <- "SY 14-15"
kcs_1415_growth$grade_level$sy <- "SY 14-15"
kcs_1415_attain$school_level$sy <- "SY 14-15"
kcs_1415_attain$grade_level$sy <- "SY 14-15"

kcs_1314_growth$school_level$sy <- "SY 13-14"
kcs_1314_growth$grade_level$sy <- "SY 13-14"
kcs_1314_attain$school_level$sy <- "SY 13-14"
kcs_1314_attain$grade_level$sy <- "SY 13-14"

growth_school <- rbind_list(kcs_1314_growth$school_level,
                            kcs_1415_growth$school_level) %>%
  select(sy,
         school,
         measurementscale,
         grades_served,
         pct_met,
         growth_pctl) %>%
  tidyr::gather(variable, value, pct_met:growth_pctl)

attain_school <- rbind_list(kcs_1314_attain$school_level,
                            kcs_1415_attain$school_level) %>%
  select(sy,
         school,
         measurementscale,
         grades_served,
         attainment_pctl) %>%
  tidyr::gather(variable, value, attainment_pctl)


sqrp_school <- rbind_list(growth_school, attain_school) %>%
  mutate(school2 = ifelse(grades_served=="2",
                                "KIPP Ascend Primary (2 only)",
                                school))



growth_grade <- rbind_list(kcs_1314_growth$grade_level,
                            kcs_1415_growth$grade_level) %>%
  select(sy,
         school = school_end,
         grade = grade_end,
         measurementscale,
         N_students,
         pct_met,
         growth_pctl) %>%
  gather(variable, value, pct_met:growth_pctl)



attain_grade <- rbind_list(kcs_1314_attain$grade_level,
                           kcs_1415_attain$grade_level) %>%
  select(sy,
         school,
         grade,
         measurementscale,
         N_students,
         attainment_pctl) %>%
  gather(variable, value, attainment_pctl)


sqrp_grade <- rbind_list(growth_grade, attain_grade)




# Plots ####

# School Level plots ####
p_schools <- ggplot(sqrp_school, aes(x=sy, y=value)) +
  geom_line(aes(group=school2,
                color=school2)) +
  geom_text(data=sqrp_school %>% filter(sy=="SY 13-14"),
            aes(label=value,
                color = school2),
            hjust=1.2,
            size = 3) +
  geom_text(data=sqrp_school %>% filter(sy=="SY 14-15"),
            aes(label=value,
                color = school2),
            hjust=-0.2,
            size = 3) +
  facet_grid(variable ~ measurementscale) +
  scale_color_discrete("School") +
  theme_bw() +
  xlab("School Year") +
  ylab('Percent(ile)') +
  ggtitle("School-level SQRP Percentiles\nfor MAP Attainment and Growth\n(estimated)")


# Grade_level Reading ####
sqrp_grade_reading <- sqrp_grade %>%
  filter(measurementscale == "Reading",
         N_students > 15) %>%
  mutate(grade = as.character(grade))

p_grade_reading <- ggplot(sqrp_grade_reading, aes(x=sy, y=value)) +
  geom_line(aes(group=grade,
                color=grade)) +
  geom_text(data=sqrp_grade_reading %>% filter(sy=="SY 13-14"),
            aes(label=value,
                color = grade),
            hjust=1.2,
            size = 3) +
  geom_text(data=sqrp_grade_reading %>% filter(sy=="SY 14-15"),
            aes(label=value,
                color = grade),
            hjust=-0.2,
            size = 3) +
  facet_grid(variable ~ school) +
  scale_color_discrete("Grade") +
  theme_bw() +
  xlab("School Year") +
  ylab('Percent(ile)') +
  ggtitle("Grade-level SQRP Percentiles\nfor MAP Attainment and Growth\nReading\n(estimated)")

p_grade_reading



# Grade-level math
sqrp_grade_math <- sqrp_grade %>%
  filter(measurementscale == "Mathematics",
         N_students > 15) %>%
  mutate(grade = as.character(grade))

p_grade_math <- ggplot(sqrp_grade_math, aes(x=sy, y=value)) +
  geom_line(aes(group=grade,
                color=grade)) +
  geom_text(data=sqrp_grade_math %>% filter(sy=="SY 13-14"),
            aes(label=value,
                color = grade),
            hjust=1.2,
            size = 3) +
  geom_text(data=sqrp_grade_math %>% filter(sy=="SY 14-15"),
            aes(label=value,
                color = grade),
            hjust=-0.2,
            size = 3) +
  facet_grid(variable ~ school) +
  scale_color_discrete("Grade") +
  theme_bw() +
  xlab("School Year") +
  ylab('Percent(ile)') +
  ggtitle("Grade-level SQRP Percentiles\nfor MAP Attainment and Growth\nMathematics\n(estimated)")

p_grade_math

cairo_pdf("graphs/Board_grade_level_analysis_151117.pdf", width=10.75, height=8.25, onefile=TRUE)
  p_schools
  p_grade_reading
  p_grade_math
dev.off()
