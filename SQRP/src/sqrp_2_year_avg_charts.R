require(dplyr)
require(tidyr)
require(readxl)
require(purrr)
require(ggplot2)

setwd("/Users/chaid/Dropbox (KIPP Chicago Schools)/Data Analysis/SQRP")


sqrp_files <- list.files("excel files/cps_data_releases/",
                         full.names = TRUE)

col_names <- c("schoolid",
              "schoolname",
              "network",

              "total_points",
              "rating",
              "accountability_status",

              "growth_all_score_reading",
              "growth_all_partic_reading",
              "growth_all_points_reading",
              "growth_all_weight_reading",

              "growth_all_score_math",
              "growth_all_partic_math",
              "growth_all_points_math",
              "growth_all_weight_math",

              "pct_me_score",
              "pct_me_partic",
              "pct_me_points",
              "pct_me_weight",

              "growth_aa_score_reading",
              "growth_aa_partic_reading",
              "growth_aa_points_reading",
              "growth_aa_weight_reading",

              "growth_hisp_score_reading",
              "growth_hisp_partic_reading",
              "growth_hisp_points_reading",
              "growth_hisp_weight_reading",

              "growth_ell_score_reading",
              "growth_ell_partic_reading",
              "growth_ell_points_reading",
              "growth_ell_weight_reading",

              "growth_dl_score_reading",
              "growth_dl_partic_reading",
              "growth_dl_points_reading",
              "growth_dl_weight_reading",

              "growth_aa_score_math",
              "growth_aa_partic_math",
              "growth_aa_points_math",
              "growth_aa_weight_math",

              "growth_hisp_score_math",
              "growth_hisp_partic_math",
              "growth_hisp_points_math",
              "growth_hisp_weight_math",

              "growth_ell_score_math",
              "growth_ell_partic_math",
              "growth_ell_points_math",
              "growth_ell_weight_math",

              "growth_dl_score_math",
              "growth_dl_partic_math",
              "growth_dl_points_math",
              "growth_dl_weight_math",

              "attain_38_score_reading",
              "attain_38_partic_reading",
              "attain_38_points_reading",
              "attain_38_weight_reading",

              "attain_38_score_math",
              "attain_38_partic_math",
              "attain_38_points_math",
              "attain_38_weight_math",

              "attain_2_score_reading",
              "attain_2_partic_reading",
              "attain_2_points_reading",
              "attain_2_weight_reading",

              "attain_2_score_math",
              "attain_2_partic_math",
              "attain_2_points_math",
              "attain_2_weight_math",

              "access_score",
              "access_partic",
              "access_points",
              "access_weight",

              "ada_score",
              "ada_points",
              "ada_weight",

              "mvms_score",
              "mvms_points",
              "mvms_weight",

              "dqi_score",
              "dqi_points",
              "dqi_weight"
              )


sqrp_data <- sqrp_files %>%
  map(~read_excel(path = ., sheet = 2,
                 skip = 4,
                 col_names = col_names
                 )
  )


sqrp_data[[1]]$sy <- "SY2014-2015"
sqrp_data[[2]]$sy <- "SY2015-2016"

sqrp_combined <- rbind_all(sqrp_data) %>%
  select(-schoolname)

#get networks data

cps_info <- readr::read_csv("data/CPS_School_Locations_SY1516.csv")

demo_col_names <- c("network",
                    "schoolid",
                    "schoolname",
                    "total_students",
                    "total_bilingual",
                    "pct_bilingual",
                    "total_sped",
                    "pct_sped",
                    "total_frm",
                    "pct_frm")


cps_demo <- read_excel("excel files/SY2015_lep_sped_frl_report_20151022.xlsx",
                       sheet = 2,
                       skip = 2,
                       col_names = demo_col_names,
                       col_types = c("text",
                                     "text",
                                     "text",
                                     "numeric",
                                     "numeric",
                                     "numeric",
                                     "numeric",
                                     "numeric",
                                     "numeric",
                                     "numeric")
                       ) %>%
  select(-network) %>%
  mutate(schoolid = as.numeric(schoolid))


incs_info <- read_excel("excel files/Schools and Networks for Chris.xlsx", na="NULL") %>%
  rename(cmo = Network)

incs_school_type <- read_excel("excel files/school_type_incs.xlsx") %>%
  rename(schoolid = CPS_ID)

sqrp <- sqrp_combined %>%
  inner_join(cps_info, by=c("schoolid" = "School_ID")) %>%
  left_join(incs_info, by=c("schoolid" = "CPS_ID")) %>%
  left_join(incs_school_type, by = "schoolid") %>%
  left_join(cps_demo, by="schoolid") %>%
  mutate(name = ifelse(is.na(cmo), schoolname, cmo))

calc_2_year <- . %>%
  filter(Grade_Cat == "ES") %>%
  mutate(kipp = grepl("kipp", tolower(name)),
         type = ifelse(kipp, "KIPP", Governance),
         type2 = ifelse(kipp, "KIPP", School_Type_Rollup),
         type3 = ifelse(type2=="Charter", "Non-selective", type2)
         ) %>%
  group_by(name, type, type2, type3) %>%
  summarize(avg_points = mean(total_points, na.rm = TRUE),
            avg_frm = mean(pct_frm, na.rm=TRUE),
            n = sum(total_students, na.rm=TRUE)) %>%
  ungroup %>%
  mutate(rank = row_number(avg_points),
         threshold_met = avg_points>=3.2,
         econ_disad = avg_frm>=.95)

sqrp_2_yr <- calc_2_year(sqrp)

threshold <- sqrp_2_yr %>%
  filter(!threshold_met) %>%
  filter(rank==max(rank))
min_points <- min(sqrp_2_yr$avg_points, na.rm = TRUE)
max_points <- max(sqrp_2_yr$avg_points, na.rm = TRUE)



# Rank Plot ####
p_rank <- ggplot(sqrp_2_yr ,
       aes(x=jitter(avg_points, factor=10),
           y=rank)) +
  geom_point(aes(color = type),
             size = 3) +
  geom_point(data = sqrp_2_yr %>%
               filter(type=="KIPP"),
             aes(x = avg_points,
                 y = rank,
                color = type2),
             size = 4) +
  geom_segment(data=sqrp_2_yr %>%
              filter(type=="KIPP"),
            aes(xend=3,
                x=avg_points-.05,
                yend=rank,
                y=rank
                ),
            color="#FEBC11",
            arrow = grid::arrow(type="closed",
                                length=grid::unit(.25, "cm"),
                                ends = "first")) +
  scale_color_manual("School Type",
                     values = c("#FEDA00",
                                "gray90",
                                "gray70",
                                "#FEBC11")) +
 # facet_grid(~econ_disad) +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  annotate("text",
           x = 3,
           y = sqrp_2_yr %>%
             filter(type=="KIPP") %>%
             select(rank) %>%
             as.numeric(),
           label = "KIPP Chicago",
           color="#FEBC11",
           hjust=1,
           vjust = .5) +
  annotate("text",
           x = 2.5,
           y = threshold$rank + .3,
           label = "3.2 point threshold",
           color="gray70",
           hjust=1,
           vjust = 0) +
  annotate("ribbon",
           x = seq(min_points, 5, by=.1),
           ymin=0,
           ymax=threshold$rank,
           alpha=.1,
           golor="gray10") +
  xlab("SQRP Points, 2-year average") +
  ggtitle(sprintf("2-year Average SQRP Scores\n%s CPS Elementary Schools",
                 nrow(sqrp_2_yr)
                 )
          )

p_rank

charter_only<-sqrp_2_yr %>% filter(type2 == "Charter")

p_rank %+% charter_only

# Scattergram with charters ####
ggplot(sqrp_2_yr %>% filter(type!="KIPP"),
       aes(y = avg_points,
           x = avg_frm)
       ) +
  geom_point(aes(color = type2,
                 y =  jitter(avg_points, factor=20)
                ),
             size = 3
             ) +
  geom_point(data = sqrp_2_yr %>%
               filter(type=="KIPP"),
             aes(y = avg_points,
                 x = avg_frm,
                 color = type2
                 ),
             size = 4.5) +
   geom_segment(data=sqrp_2_yr %>%
                  filter(type=="KIPP"),
                aes(yend=4.75,
                    yend=avg_points-.05,
                    x=avg_frm-.005,
                    xend=.6
                ),
                color="#FEBC11",
                arrow = grid::arrow(type="closed",
                                    length=grid::unit(.25, "cm"),
                                    ends = "first")) +
  scale_color_manual("School Type",
                     values = c("#FEDA00",
                                "#FEBC11",
                                "gray90",
                                "gray60"
                                )) +
#  scale_size_area("Students Served", max_size  = 7, breaks=c(300, 1000, 2000)) +
#   #facet_grid(~econ_disad) +
  theme_bw() +
#   theme(axis.ticks.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.title.y = element_blank()) +
   annotate("text",
           y = 4.75,
           x = .6,
           label = "KIPP Chicago",
           color="#FEBC11",
           hjust=1,
           vjust = .3) +
  annotate("text",
           x = .5,
           y = 3.2,
           label = "3.2 point threshold",
           color="gray70",
           hjust=1,
           vjust = 0) +
  annotate("ribbon",
           x = seq(.1, 1, by=.05),
           ymin=1,
           ymax=3.2,
           alpha=.1,
           golor="gray10") +
  xlab("Percent Economically Disadvantaged Students") +
  ylab("2-year SQRP Average (SY13-14 & SY 14-15)") +
  ggtitle(sprintf("2-year Average SQRP Scores\n%s CPS Elementary Schools versus\nPercent of Students Economically Disadvantaged",
                  nrow(sqrp_2_yr %>%
                         filter(!is.na(avg_points))
                         )
                  )
  )


# Scattergram WITHOUT charters ####
ggplot(sqrp_2_yr %>% filter(type!="KIPP"),
       aes(y = avg_points,
           x = avg_frm)
) +
  geom_point(aes(color = type3,
                 y =  jitter(avg_points, factor=20)
  ),
  size = 3
  ) +
  geom_point(data = sqrp_2_yr %>%
               filter(type=="KIPP"),
             aes(y = avg_points,
                 x = avg_frm,
                 color = type3
             ),
             size = 4.5) +
  geom_segment(data=sqrp_2_yr %>%
                 filter(type=="KIPP"),
               aes(yend=4.75,
                   yend=avg_points-.05,
                   x=avg_frm-.005,
                   xend=.6
               ),
               color="#FEBC11",
               arrow = grid::arrow(type="closed",
                                   length=grid::unit(.25, "cm"),
                                   ends = "first")) +
  scale_color_manual("School Type",
                     values = c(
                                "#FEBC11",
                                "gray90",
                                "gray60"
                     )) +
  #  scale_size_area("Students Served", max_size  = 7, breaks=c(300, 1000, 2000)) +
  #   #facet_grid(~econ_disad) +
  theme_bw() +
  #   theme(axis.ticks.y = element_blank(),
  #         axis.text.y = element_blank(),
  #         axis.title.y = element_blank()) +
  annotate("text",
           y = 4.75,
           x = .6,
           label = "KIPP Chicago",
           color="#FEBC11",
           hjust=1,
           vjust = .3) +
  annotate("text",
           x = .5,
           y = 3.2,
           label = "3.2 point threshold",
           color="gray70",
           hjust=1,
           vjust = 0) +
  annotate("ribbon",
           x = seq(.1, 1, by=.05),
           ymin=1,
           ymax=3.2,
           alpha=.1,
           golor="gray10") +
  xlab("Percent Economically Disadvantaged Students") +
  ylab("2-year SQRP Average (SY13-14 & SY 14-15)") +
  ggtitle(sprintf("2-year Average SQRP Scores\n%s CPS Elementary Schools versus\nPercent of Students Economically Disadvantaged",
                  nrow(sqrp_2_yr %>%
                         filter(!is.na(avg_points))
                  )
  )
  )


# Bar/lines ####


geom_kipp_bar <- geom_segment(data = sqrp_2_yr %>% filter(type3 == "KIPP"),
                              aes(xend=avg_points,
                                  x=0,
                                  yend=rank,
                                  y=rank,
                                  color = type3),
                              size = 1
)

p_bar <- ggplot(sqrp_2_yr ,
                 aes(x=jitter(avg_points, factor=10),
                     y=rank)) +
  geom_segment(aes(xend=avg_points,
                   x=0,
                   yend=rank,
                   y=rank,
                   color = type2
                   ),
               size = .5
               ) +
  scale_color_manual("School Type",
                     values = c("#FEDA00",
                                "#FEBC11",
                                "gray90",
                                "gray70"
                                )) +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  xlab("SQRP Points, 2-year average")

p_bar + geom_kipp_bar

# DATA BREAKOUTS####

# KIPP Neighborhoods ####

kipp_hoods <- c("ENGLEWOOD", "NORTH LAWNDALE", "AUSTIN")


sqpr_kipp_hoods <- sqrp %>%
  filter(COMMAREA %in% kipp_hoods) %>%
  calc_2_year

ggplot(sqpr_kipp_hoods ,
       aes(x=jitter(avg_points, factor=10),
           y=rank)) +
  geom_segment(aes(xend=avg_points,
                   x=0,
                   yend=rank,
                   y=rank,
                   color = type2
                   ),
               size = 4.5
               ) +
  geom_text(aes(x=0, y= rank,
                label = name),
            hjust =0,
            size = 4) +
  scale_color_manual("School Type",
                     values = c("#FEDA00",
                                "#FEBC11",
                                "gray90",
                                "gray70"
                     )) +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  xlab("SQRP Points, 2-year average")

# AA ####
sqpr_kipp_hoods <- sqrp %>%
  filter() %>%
  calc_2_year



# year over year change in SQRP points ####

sqrp_2 <-sqrp   %>%
  select(schoolid, schoolname, School_Type_Rollup, total_points, sy) %>%
  spread(key=sy, value=total_points) %>%
  rename(sy14 = `SY2014-2015`,
         sy15 = `SY2015-2016`,
         type=School_Type_Rollup) %>%
  mutate(diff = sy15 - sy14,
         pos = diff>=0) %>%
  as.data.frame()

ggplot(sqrp_2 %>% filter(!is.na(pos))) +
  geom_segment(aes(y = sy14,
                   yend = sy15,
                   x=0,
                   xend=1,
                   color=pos),
                   alpha = .25) +
  facet_grid(type~pos)


sqrp_2_ada <-sqrp   %>%
  select(schoolid, schoolname, School_Type_Rollup, ada_score, sy) %>%
  spread(key=sy, value=ada_score) %>%
  rename(sy14 = `SY2014-2015`,
         sy15 = `SY2015-2016`,
         type=School_Type_Rollup) %>%
  mutate(diff = sy15 - sy14,
         pos = diff>=0) %>%
  as.data.frame()

ggplot(sqrp_2_ada %>% filter(!is.na(pos))) +
  geom_segment(aes(y = sy14,
                   yend = sy15,
                   x=0,
                   xend=1,
                   color=pos),
               alpha = .25) +
  facet_grid(type~pos)






sqrp_2_ranked <- sqrp  %>%
  group_by(sy) %>%
  mutate(rank_1_year = dense_rank(total_points))


ggplot(sqrp_2_ranked, aes())  +
  geom_segment(aes(group=schoolid), alpha=.1)
