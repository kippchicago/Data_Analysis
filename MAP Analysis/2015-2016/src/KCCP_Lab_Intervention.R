# KCCP numbers for 7th/8th graders for Lab Class.


require(ProjectTemplate)
load.project()
require(googlesheets)


# Get 7th and 8th graders
names(current_ps_roster) <- names(current_ps_roster) %>%  tolower()

kccp_7<- current_ps_roster %>%
  filter(
    grade_level == 7,
    schoolid == 400146
  ) %>%
  select(
    studentid,
    grade_level,
    home_room
  )

kccp_8<- current_ps_roster %>%
  filter(
    grade_level == 8,
    schoolid == 400146
  ) %>%
  select(
    studentid,
    grade_level,
    home_room
  )

kccp_7_8 <- bind_rows(kccp_7, kccp_8)

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



p_7_reading <-
  scatter2(
    map_viz,
    studentids = kccp_7$studentid,
    measurementscale = "Reading",
    start_fws = "Fall",
    start_academic_year = 2015,
    end_fws = "Winter",
    end_academic_year = 2015
    ) +
  ggtitle("KCCP 7th Grade Reading\nGrowth vs Attainment Percentile\nFall 15 to Winter 16")

p_7_math <-
  scatter2(
    map_viz,
    studentids = kccp_7$studentid,
    measurementscale = "Mathematics",
    start_fws = "Fall",
    start_academic_year = 2015,
    end_fws = "Winter",
    end_academic_year = 2015
  ) +
  ggtitle("KCCP 7th Grade Math\nGrowth vs Attainment Percentile\nFall 15 to Winter 16")


p_8_reading <-
  scatter2(
    map_viz,
    studentids = kccp_8$studentid,
    measurementscale = "Reading",
    start_fws = "Fall",
    start_academic_year = 2015,
    end_fws = "Winter",
    end_academic_year = 2015
  ) +
  ggtitle("KCCP 8th Grade Reading\nGrowth vs Attainment Percentile\nFall 15 to Winter 16")

p_8_math <-
  scatter2(
    map_viz,
    studentids = kccp_8$studentid,
    measurementscale = "Mathematics",
    start_fws = "Fall",
    start_academic_year = 2015,
    end_fws = "Winter",
    end_academic_year = 2015
  ) +
  ggtitle("KCCP 8th Grade Math\nGrowth vs Attainment Percentile\nFall 15 to Winter 16")


# Amy's list

p_sl_7_reading <-
  strands_list_plot(
    map_viz,
    kccp_7$studentid,
    measurement_scale = "Reading",
    season = "Winter",
    year = 2015
    ) +
  ggtitle("KCCP 7th Grade Reading | Goal Strands | Winter 2016")

p_sl_7_math <-
  strands_list_plot(
    map_viz,
    kccp_7$studentid,
    measurement_scale = "Mathematics",
    season = "Winter",
    year = 2015
  ) +
  ggtitle("KCCP 7th Grade Math | Goal Strands | Winter 2016")

p_sl_8_reading <-
  strands_list_plot(
    map_viz,
    kccp_8$studentid,
    measurement_scale = "Reading",
    season = "Winter",
    year = 2015
  ) +
  ggtitle("KCCP 8th Grade Reading | Goal Strands | Winter 2016")

p_sl_8_math <-
  strands_list_plot(
    map_viz,
    kccp_8$studentid,
    measurement_scale = "Mathematics",
    season = "Winter",
    year = 2015
  ) +
  ggtitle("KCCP 8th Grade Math | Goal Strands | Winter 2016")

# other strand plot

require(purrr)


p_gs_7_reading <- split(kccp_7, kccp_7$home_room) %>%
  map(~goal_strand_plot(
        map_viz,
        studentids = .$studentid,
        measurementscale = "Reading",
        fws = "Winter",
        year = 2015
      ) + ggtitle(sprintf("KCCP  %s | Reading | Winter 2016",
                         unique(.$home_room)
                         )
                 )
    )

p_gs_7_math <- split(kccp_7, kccp_7$home_room) %>%
  map(~goal_strand_plot(
    map_viz,
    studentids = .$studentid,
    measurementscale = "Mathematics",
    fws = "Winter",
    year = 2015
  ) + ggtitle(sprintf("KCCP  %s | Math | Winter 2016",
                      unique(.$home_room)
                      )
              )
  )

p_gs_8_reading <- split(kccp_8, kccp_8$home_room) %>%
  map(~goal_strand_plot(
    map_viz,
    studentids = .$studentid,
    measurementscale = "Reading",
    fws = "Winter",
    year = 2015
  ) + ggtitle(sprintf("KCCP  %s | Reading | Winter 2016",
                      unique(.$home_room)
  )
  )
  )

p_gs_8_math <- split(kccp_8, kccp_8$home_room) %>%
  map(~goal_strand_plot(
    map_viz,
    studentids = .$studentid,
    measurementscale = "Mathematics",
    fws = "Winter",
    year = 2015
  ) + ggtitle(sprintf("KCCP  %s | Math | Winter 2016",
                      unique(.$home_room)
  )
  )
  )



# lets wrapt this up in a pdf

cairo_pdf("figure/KCCP_Labs.pdf", width = 10.8, height = 8.48, onefile = TRUE)

  p_7_reading
  p_7_math

  p_8_reading
  p_8_math

  p_sl_7_reading
  p_sl_7_math

  p_sl_8_reading
  p_sl_8_math

  p_gs_7_reading
  p_gs_7_math

  p_gs_8_reading
  p_gs_8_math

dev.off()

# get current students spring to winter as percent of spring to spring
map_spring15 <- map_viz$growth_df %>%
  inner_join(kccp_7_8, by = "studentid") %>%
  filter(measurementscale %in% c("Reading", "Mathematics"),
         start_map_year_academic >= 2014,
         start_fallwinterspring == "Spring",
         end_fallwinterspring == "Spring")

# get current students spring to winter results
map_winter16 <- map_viz$cdf %>%
  inner_join(kccp_7_8, by = "studentid") %>%
  filter(
    measurementscale %in% c("Reading", "Mathematics"),
    map_year_academic >= 2015,
    fallwinterspring == "Winter"
    ) %>%
  select(studentid,
         measurementscale,
         winter_testritscore = testritscore,
         winter_testpercentile = testpercentile,
         winter_consistent_percentile = consistent_percentile)

# join spring with goals to winter

map_kccp_7_8 <- map_spring15 %>%
  inner_join(
    map_winter16,
    by = c("studentid", "measurementscale")
    )

map_sw_growth <- map_kccp_7_8 %>%
  mutate(
    rit_growth = winter_testritscore - start_testritscore,
    magnitude_of_growth = rit_growth/reported_growth,
    cgi = (rit_growth - reported_growth)/std_dev_of_expectation,
    sgp = pnorm(cgi)*100
    ) %>%
  select(
    studentid,
    measurementscale,
    grade_level,
    spring_testritscore = start_testritscore,
    winter_testritscore,
    rit_growth,
    reported_growth,
    magnitude_of_growth,
    cgi,
    sgp
    ) %>%
  left_join(current_ps_roster %>%
              select(studentid,
                     studentlastname,
                     studentfirstname,
                     home_room),
            by = "studentid") %>%
  group_by(measurementscale, grade_level) %>%
  mutate(rank = row_number(spring_testritscore))


# Let's do some plotting

new_plot <- function(subj, grade){
  ggplot(map_sw_growth %>%
           filter(grade_level == grade,
                  measurementscale == subj),
         aes(x=cgi,
             y=spring_testritscore)) +
    geom_vline(aes(xintercept = 0), color = "red") +
    geom_text_repel(aes(label = sprintf("%s %s",
                                        studentfirstname,
                                        studentlastname
                                        )
                        ),
                    size = rel(2.5), max.iter = 1000
                    ) +
    geom_point() +
    facet_grid(grade_level ~ measurementscale) +
    theme_bw() +
    labs(y = "Spring 15 RIT Score",
         x= "Conditional Growth Index\n(CGI >= 0 means typical growth exceeded)")
}

# save data to spreadsheet

map_gs <- map_sw_growth %>%
  arrange(measurementscale, grade_level, home_room, studentlastname, studentfirstname) %>%
  select(Subject = measurementscale,
         Grade = grade_level,
         StudentID = studentid,
         "Home Room"= home_room,
         "Last Name" = studentlastname,
         "First Name" = studentfirstname,
         "RIT Spring" = spring_testritscore,
         "RIT Winter" = winter_testritscore,
         "RIT Growth" = rit_growth,
         "Typical Growth" = reported_growth,
         "Magnitude of Growth" = magnitude_of_growth,
         "Conditional Growth Index" = cgi,
         "Student Growth Percentile" = sgp
         )

cairo_pdf(filename = "figure/KCCP_Labs_2.pdf", onefile = TRUE,
          width = 10.8, height = 8.3)

new_plot("Reading", 7) +
  ggtitle("KCCP 7th Grade Reading\nSpring RIT vs Spring to Winter CGI\n(cond'l on Spring to Spring Growth)")
new_plot("Mathematics", 7) +
  ggtitle("KCCP 7th Grade Mathematics\nSpring RIT vs Spring to Winter CGI\n(cond'l on Spring to Spring Growth)")
new_plot("Reading", 8) +
  ggtitle("KCCP 8th Grade REading\nSpring RIT vs Spring to Winter CGI\n(cond'l on Spring to Spring Growth)")
new_plot("Mathematics", 8) +
  ggtitle("KCCP 8th Grade Mathematics\nSpring RIT vs Spring to Winter CGI\n(cond'l on Spring to Spring Growth)")


dev.off()



gs <- gs_new("KCCP Labs MAP Data")

#gs %>% gs_delete()




gs %>%  gs_ws_new("2020 (8th grade)", input = map_gs[map_gs$Grade == 8,])

gs %>% gs_ws_new("2021 (7th grade)", input = map_gs[map_gs$Grade == 7,])

