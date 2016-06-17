# how much growth do we need to make to make different goals.

# Step one:  look at how different multiples on spring-to-winter lead
# to different % M/E and check that % M/E is consistent with M/E growth (as
# well as just RIT growth)

# Load Packages ####

require(sqrpr)
require(dplyr)
require(mapvizieR)
require(RSQLServer)
require(stringr)
require(purrr)
require(ggplot2)

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
  filter(TermName %in% c("Spring 2014-2015", "Winter 2015-2016"),
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

map_viz_2015_norms<-mapvizieR(map_sep$cdf, map_sep$roster, include_unsanctioned_windows=TRUE,
                              norm_df_long = mapvizieR:::norms_students_wide_to_long(student_growth_norms_2015))


# read sped csv and join to
sped <- readr::read_csv("data/sped_W16.csv", trim_ws = T)

sped <- sped[,1:2] #dorppping na columns

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

map_cdf_S16_projected <- grow_by_factor(map_viz_2015_norms,
                                        growth_factor = 1.25,
                                        neg_growth_adj = 0
                                        ) %>%
  left_join(map_viz_2015_norms$roster %>%
              filter(termname=="Winter 2015-2016") %>%
              select(studentid, studentethnicgroup, sped),
            by="studentid")



map_sqrp_proj <- bind_rows(map_cdf_S15, map_cdf_S16_projected)



#map_sqrp_proj <- map_sqrp_proj %>% left_join(sped, by="studentid")



kcs_growth <- school_growth_percentile(map_sqrp_proj)

#calculate_attainment
kcs_attain <- school_attainment_percentile(map_cdf_S16_projected)


#calculate growth priority groups ####
#AA
kcs_growth_aa <- priority_group(kcs_growth,
                              group_column = "studentethnicgroup",
                              group_id = "Black or African American"
)

#IEP
kcs_growth_dl <- priority_group(kcs_growth,
                              group_column = "sped",
                              group_id = "TRUE"
)

# pct m/e
kcs_pct_me<-calc_pct_me(kcs_growth)




schools<- c("Ascend", "Create", "Bloom")

sqrp_results<-schools %>%
  map(~sqrp_level(.,
                 kcs_growth,
                 kcs_attain,
                 kcs_growth_aa,
                 kcs_growth_dl,
                 kcs_pct_me,
                 ada=.96,
                 mvms="WO",
                 dqi=.99)
  ) %>%
  bind_rows %>%
  mutate(school = schools)

kcs_pct_me %>% ungroup %>%
  mutate(school = str_extract(school, "Ascend|Bloom|Create")) %>%
  select(school, pct_met) %>%
  inner_join(sqrp_results, by = "school")


estimate_sqrp_by_growth(map_viz_2015_norms)

# purrring this along
sqpr_growth_table_0<-
  seq(0,3, by=.1) %>%
  map(~
    estimate_sqrp_by_growth(
      mv_obj = map_viz_2015_norms,
      growth_factor = .,
      neg_growth_adj = 0,
      ada = .96,
      mvms = "WO",
      dqi = .99
      )
    ) %>%
  bind_rows()

sqpr_growth_table_expected<-
  seq(0,3, by=.1) %>%
  map(~
        estimate_sqrp_by_growth(
          mv_obj = map_viz_2015_norms,
          growth_factor = .,
          neg_growth_adj = "reported",
          ada = .96,
          mvms = "WO",
          dqi = .99
        )
  ) %>%
  bind_rows()


sqpr_growth_table <- bind_rows(sqpr_growth_table_0,
                               sqpr_growth_table_expected) %>%
  mutate(neg_type = ifelse(neg_growth_adjst_type=="0",
                           "Negative = 0", "Negative =\n 1/2 Spring-to-Spring"))


ggplot(sqpr_growth_table,
       aes(
        x = growth_factor,
        y = points
        )
       ) +
  geom_ribbon(aes(ymin=2, ymax=3), alpha=.2, fill = "lightgray") +
  geom_ribbon(aes(ymin=3, ymax=3.5), alpha=.2, fill = "lightblue") +
  geom_ribbon(aes(ymin=3.5, ymax=4), alpha=.2, fill = "lightgreen") +
  geom_ribbon(aes(ymin=4, ymax=5), alpha=.2, fill = "green") +

  annotate("text", x=0.1, y = 3.05, label = "Level 2+", color = "lightblue", hjust = 0, vjust = 0, size=3) +
  annotate("text", x=0.1, y = 3.55, label = "Level 1", color = "lightgreen", hjust = 0, vjust = 0, size=3) +
  annotate("text", x=0.1, y = 4.05, label = "Level 1+", color = "green", hjust = 0, vjust = 0, size = 3) +

  geom_line(aes(color = school)) +
  geom_smooth(aes(color = school), span = .75 ) +

  facet_grid(.~neg_type) +
  theme_minimal() +
  labs(x= "Growth Factor\n(W2S÷S2W)",
       y= "SQRP Points",
       color = "School")



# how about previous years growth factors  ####


map_silo_3 <-
  map_silo %>%
  filter(
         MeasurementScale %in% c("Mathematics", "Reading"))

#seperate cdf
map_sep_3 <- separate_cdf(map_silo_3, district_name = "KIPP Chicago")

map_sep_3$cdf <- map_sep_3$cdf %>%
  mutate(
    school_initials = abbrev(SchoolName,
                             exceptions = list(old = "KAPS", new = "KAP")
    ),
    SchoolName = ifelse(grepl("Ascend", SchoolName), "KIPP Ascend", SchoolName)
  )




#map_viz_2011_norms<-mapvizieR(map_sep$cdf, map_sep$roster, include_unsanctioned_windows=TRUE,
#                              norm_df_long = mapvizieR:::norms_students_wide_to_long(student_growth_norms_2011))

map_viz_3<-mapvizieR(map_sep_3$cdf, map_sep$roster, include_unsanctioned_windows=TRUE,
                              norm_df_long = mapvizieR:::norms_students_wide_to_long(student_growth_norms_2015))

map_growth_s2w <-map_viz_3$growth_df %>%
  filter(growth_window %in% c("Spring to Winter"),
         end_map_year_academic >= 2013,
         complete_obsv) %>%
  select(studentid,
         end_schoolname,
         end_map_year_academic,
         measurementscale,
         growth_s2w = rit_growth) %>%
  mutate(growth_s2w_0 = ifelse(growth_s2w<0, 0, growth_s2w))

map_growth_w2s <-map_viz_3$growth_df %>%
  filter(growth_window %in% c("Winter to Spring"),
         end_map_year_academic >= 2013,
         complete_obsv) %>%
  select(studentid,
         end_schoolname,
         end_map_year_academic,
         measurementscale,
         growth_w2s = rit_growth)


map_growth<-map_growth_s2w %>%
  inner_join(map_growth_w2s,
             by = c("studentid",
                    "end_schoolname",
                    "end_map_year_academic",
                    "measurementscale")
             )

map_growth %>%
  group_by(end_map_year_academic, end_schoolname) %>%
summarize(mean_growth_s2w = mean(growth_s2w),
          mean_growth_s2w_0 = mean(growth_s2w_0),
          mean_growth_w2s = mean(growth_w2s)) %>%
  mutate(growth_factor=mean_growth_w2s/mean_growth_s2w,
         growth_factor_0=mean_growth_w2s/mean_growth_s2w_0)


