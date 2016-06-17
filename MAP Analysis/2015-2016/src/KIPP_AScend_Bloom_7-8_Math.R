# KCCP numbers for 7th/8th graders for Lab Class.


require(ProjectTemplate)
load.project()
require(googlesheets)


# Get 7th and 8th graders
names(current_ps_roster) <- names(current_ps_roster) %>%  tolower()

ascend_7<- current_ps_roster %>%
  filter(
    grade_level == 7, # 7th and 8th
    schoolid == 7810
  ) %>%
  select(
    studentid,
    grade_level,
    home_room
  )

ascend_8<- current_ps_roster %>%
  filter(
    grade_level == 8, # 7th and 8th
    schoolid == 7810
  ) %>%
  select(
    studentid,
    grade_level,
    home_room
  )


bloom_7<- current_ps_roster %>%
  filter(
    grade_level == 7,
    schoolid == 400163
  ) %>%
  select(
    studentid,
    grade_level,
    home_room
  )


# run scatter
mapvizieR::growth_status_scatter(
  map_viz,
  studentids = kccp_8$studentid,
  measurementscale = "Reading",
  start_fws = "Fall",
  start_academic_year = 2015,
  end_fws = "Winter",
  end_academic_year = 2015)


# Rewrite the ggrepel
scatter2 <- function (mapvizieR_obj, studentids, measurementscale, start_fws,
                      start_academic_year, end_fws, end_academic_year) {

  measurementscale_in <- measurementscale
  goal_df <- mapvizieR_obj[["growth_df"]] %>%
    dplyr::filter(
      measurementscale == measurementscale_in,
      studentid %in% studentids,
      end_map_year_academic == end_academic_year,
      end_fallwinterspring == end_fws,
      start_map_year_academic == start_academic_year,
      start_fallwinterspring == start_fws)

  goal_df <- goal_df %>%
    dplyr::left_join(mapvizieR_obj$roster %>%
                       dplyr::filter(studentid %in% studentids) %>%
                       dplyr::select(studentid, studentfirstlast) %>%
                       unique(),
                     by = "studentid")

  annotation_df <- data.frame(lab_x = c(33/2, 50, 66 + 33/2,
                                        33/2, 50, 66 + 33/2), lab_y = c(75, 75, 75, 25, 25, 25),
                              lab_text = c("Low Growth\nAbove Gr. Lev.", "Avg Growth\nAbove Gr. Lev.",
                                           "High Growth\nAbove Gr. Lev.", "Low Growth\nBelow Gr. Lev.",
                                           "Avg Growth\nBelow Gr. Lev.", "High Growth\nBelow Gr. Lev."))
  p <- ggplot(data = goal_df,
              aes(x = sgp * 100,
                  y = end_testpercentile)
              ) +
    annotate(geom = "text",
             x = annotation_df$lab_x,
             y = annotation_df$lab_y,
             label = annotation_df$lab_text,
             size = 9,
             color = "gray80",
             alpha = 0.8
    ) +
    geom_vline(xintercept = c(34, 66),
               size = 0.5,
               color = "gray50",
               alpha = 0.6
    ) +
    geom_hline(yintercept = 50,
               size = 0.5,
               color = "gray50",
               alpha = 0.6
    ) +
  geom_point(size = 2,
             color = "red") +
    geom_text_repel(aes(label = studentfirstlast),
                    max.iter = 100,
                    box.padding = unit(0.37, "lines"),
                    force = 1,
                    segment.color = "red"
    ) +

    #geom_jitter(size = 2,
               # shape = 1,
               # position = position_jitter(height = 0.75,
               #                            width = 0.75),
               # alpha = 0.4, color = "gray50") +
    coord_cartesian(ylim = c(0, 100),
                    xlim = c(0, 100)) +
    scale_x_continuous(breaks = seq(10, 90, by = 10),
                       minor_breaks = NULL) +
    scale_y_continuous(breaks = seq(10, 90, by = 10),
                       minor_breaks = NULL
                       ) +
    labs(x = "Growth Percentile",
         y = "Percentile Rank") +
    theme(plot.title = element_text(hjust = 0,
                                    face = "bold",
                                    size = 20
                                    ),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = "gray95",
                                          linetype = "longdash",
                                          size = 0.25))
  #return p
  p
}



p_7_ascend_math <-
  scatter2(
    map_viz,
    studentids = ascend_7$studentid,
    measurementscale = "Mathematics",
    start_fws = "Fall",
    start_academic_year = 2015,
    end_fws = "Winter",
    end_academic_year = 2015
    ) +
  ggtitle("Ascend 7th Grade Math\nGrowth vs Attainment Percentile\nFall 15 to Winter 16")

p_8_ascend_math <-
  scatter2(
    map_viz,
    studentids = ascend_8$studentid,
    measurementscale = "Mathematics",
    start_fws = "Fall",
    start_academic_year = 2015,
    end_fws = "Winter",
    end_academic_year = 2015
  ) +
  ggtitle("Ascend 8th Grade Math\nGrowth vs Attainment Percentile\nFall 15 to Winter 16")

p_7_bloom_math <-
  scatter2(
    map_viz,
    studentids = bloom_7$studentid,
    measurementscale = "Mathematics",
    start_fws = "Fall",
    start_academic_year = 2015,
    end_fws = "Winter",
    end_academic_year = 2015
  ) +
  ggtitle("Bloom 7th Grade Math\nGrowth vs Attainment Percentile\nFall 15 to Winter 16")


# Amy's list

p_sl_7_ascend_math <-
  strands_list_plot(
    map_viz,
    ascend_7$studentid,
    measurement_scale = "Mathematics",
    season = "Winter",
    year = 2015
    ) +
  ggtitle("Ascend 7th Grade Math | Goal Strands | Winter 2016")

p_sl_8_ascend_math <-
  strands_list_plot(
    map_viz,
    ascend_8$studentid,
    measurement_scale = "Mathematics",
    season = "Winter",
    year = 2015
  ) +
  ggtitle("Ascend 8th Grade Math | Goal Strands | Winter 2016")

p_sl_7_bloom_math <-
  strands_list_plot(
    map_viz,
    bloom_7$studentid,
    measurement_scale = "Mathematics",
    season = "Winter",
    year = 2015
  ) +
  ggtitle("Bloom 7th Grade Math | Goal Strands | Winter 2016")




# lets wrapt this up in a pdf

cairo_pdf("figure/Ascend_Bloom_7_8_Winter.pdf", width = 10.8, height = 8.48, onefile = TRUE)

p_7_ascend_math
p_sl_7_ascend_math

p_8_ascend_math
p_sl_8_ascend_math


p_7_bloom_math
p_sl_7_bloom_math

dev.off()

