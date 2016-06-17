

# Load Packages ####

require(sqrpr)
require(dplyr)
require(mapvizieR)
require(RSQLServer)
require(stringr)
require(purrr)
require(ggplot2)
require(googlesheets)

source('lib/growth_model.R')
# Get MAP Data
silo <- as.data.frame(read.dcf('config//silo_dw.dcf'))

host <- str_extract(silo$url, "\\d+\\.\\d+\\.\\d+\\.\\d+")
port <- str_extract(silo$url, "\\d{4}$")

silo_db <- src_sqlserver(server = host,
                         port = port,
                         database = silo$dbname,
                         properties = list(
                           user = silo$user,
                           password = silo$password
                           )
                         )

qry <- "SELECT * FROM NWEA..MAP$comprehensive#plus_cps WHERE GrowthMeasureYN='TRUE'"

map_silo <- collect(tbl(silo_db, sql(qry)))

map_silo_2 <-
  map_silo %>%
  filter(TermName %in% c("Spring 2014-2015", "Spring 2015-2016", "Fall 2015-2016"),
         MeasurementScale %in% c("Mathematics", "Reading"))

#seperate cdf
map_sep <- separate_cdf(map_silo_2, district_name = "KIPP Chicago")

map_sep$cdf <- map_sep$cdf %>%
  mutate(
    school_initials = abbrev(SchoolName,
                             exceptions = list(old = "KAPS", new = "KAP")
                             ),
    SchoolName = ifelse(grepl("Ascend", SchoolName), "KIPP Ascend", SchoolName)
  )




#map_viz_2011_norms<-mapvizieR(map_sep$cdf, map_sep$roster, include_unsanctioned_windows=TRUE,
#                              norm_df_long = mapvizieR:::norms_students_wide_to_long(student_growth_norms_2011))

map_viz_2015_norms<-mapvizieR(map_sep$cdf, map_sep$roster, include_unsanctioned_windows=FALSE,
                              norm_df_long = mapvizieR:::norms_students_wide_to_long(student_growth_norms_2015))


# read sped csv and join to
# sped <- readr::read_csv("data/sped_W16.csv", trim_ws = T)
# sped <- sped[,1:2] #dorppping na columns

gsheet <- gs_auth()

sped <- gs_title("LRE/Disability Category ") %>%
  gs_read(ws=1)

sped <- sped %>% select(studentid = `Student ID`) %>%
  mutate(sped = TRUE)





map_viz_2015_norms$roster <-
  map_viz_2015_norms$roster %>% left_join(sped, by="studentid")


# start function here ####



map_cdf_S15 <- map_viz_2015_norms$cdf %>%
  filter(termname == "Spring 2014-2015") %>%
  left_join(
    map_viz_2015_norms$roster %>%
      select(studentid, termname, studentethnicgroup, sped),
    by = c("studentid", "termname")
    )

# map_cdf_S16_projected <- grow_by_factor(map_viz_2015_norms,
#                                         growth_factor = 1.25,
#                                         neg_growth_adj = 0
#                                         ) %>%
#   left_join(map_viz_2015_norms$roster %>%
#               filter(termname=="Winter 2015-2016") %>%
#               select(studentid, studentethnicgroup, sped),
#             by="studentid")

map_cdf_S16 <- map_viz_2015_norms$cdf %>%
  filter(termname == "Spring 2015-2016") %>%
  left_join(
    map_viz_2015_norms$roster %>%
      select(studentid, termname, studentethnicgroup, sped),
    by = c("studentid", "termname")
  )

map_cdf_F15 <- map_viz_2015_norms$cdf %>%
  filter(termname == "Fall 2015-2016") %>%
  left_join(
    map_viz_2015_norms$roster %>%
      select(studentid, termname, studentethnicgroup, sped),
    by = c("studentid", "termname")
  ) %>%
  select(studentid, measurementscale, testritscore, grade)




map_sqrp_proj <- bind_rows(map_cdf_S15, map_cdf_S16)


kcs_growth <- school_growth_percentile(map_sqrp_proj, fall_equate_scores = map_cdf_F15)

#calculate_attainment
kcs_attain <- school_attainment_percentile(map_cdf_S16_projected)


#calculate growth priority groups ####
#AA
kcs_growth_aa <- priority_group(kcs_growth,
                              group_column = "studentethnicgroup",
                              group_id = "Black or African American",
                              fall_equate_scores = map_cdf_F15
)

#IEP
kcs_growth_dl <- priority_group(kcs_growth,
                              group_column = "sped",
                              group_id = "TRUE",
                              fall_equate_scores = map_cdf_F15
)

# pct m/e
kcs_pct_me<-calc_pct_me(kcs_growth)




# KACP ##

sqrp_kacp<-sqrp_level("Ascend",
                      kcs_growth,
                      kcs_attain,
                      kcs_growth_aa,
                      kcs_growth_dl,
                      kcs_pct_me,
                      ada=.95,
                      mvms="O",
                      dqi=.99
) %>% mutate(school="KACP")

# KCCP ##

sqrp_kccp<-sqrp_level("Create",
                      kcs_growth,
                      kcs_attain,
                      kcs_growth_aa,
                      kcs_growth_dl,
                      kcs_pct_me,
                      ada=.95,
                      mvms="NYO",
                      dqi=.99
)%>% mutate(school="KCCP")

# KBCP ##

sqrp_kbcp<-sqrp_level("Bloom",
                      kcs_growth,
                      kcs_attain,
                      kcs_growth_aa,
                      kcs_growth_dl,
                      kcs_pct_me,
                      ada=.95,
                      mvms="WO",
                      dqi=.5
) %>% mutate(school="KBCP")

rbind_list(sqrp_kacp, sqrp_kccp, sqrp_kbcp)
