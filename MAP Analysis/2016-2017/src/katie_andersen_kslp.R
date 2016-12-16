# Katy Andersen Request

library(silounloadr)
library(mapvizieR)
library(dplyr)
library(purrr)
library(xlsx)
library(janitor)
library(ggrepel)



# Function to separte combined cdf to roster and results
separate_cdf <- function(combinded_cdf, district_name = "Not provided"){
  ar_names <- names(ex_CombinedAssessmentResults)
  stu_names <- names(ex_CombinedStudentsBySchool)

  if (!"districtname" %in% tolower(names(combinded_cdf))) {
    combinded_cdf <- combinded_cdf %>% mutate_(DistrictName = ~district_name)
  }

  roster<-combinded_cdf %>%
    select_(.dots = stu_names) %>%
    unique

  cdf<-combinded_cdf %>% select(-StudentLastName:-StudentFirstName,
                                -StudentMI:-StudentGender,
                                -Grade) %>%
    mutate(TestID=as.character(TestID))

  out <- list(cdf = cdf,
              roster = roster)

}




subjs <- c("Reading", "Mathematics", "General Science")

nwea <- get_nwea(table_name = "MAP$comprehensive#plus_cps")

nwea <- collect(nwea)

nwea<-nwea %>% select(-ClassName, -TeacherName) %>% distinct()



cdf <- separate_cdf(nwea, "KIPP Chicago")

map_mv<-mapvizieR(cdf = cdf$cdf, roster = cdf$roster)

map_mv$roster %>% glimpse

test_roster <- map_mv$roster %>%
  filter(grepl("Ascend Primary", schoolname), map_year_academic == 2015)

z <- test_roster %>% filter(grade==4)


##### Plots ######

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




hp <- haid_plot(map_mv,
          studentids = unique(z$studentid),
          measurementscale = "Mathematics",
          start_fws = "Spring",
          start_academic_year = 2014,
          end_fws = "Spring",
          end_academic_year = 2015) +
  labs(title = "Waterfall | KAP | 4th grade | Math | Spring 15 - Spring 16")

npr_p <- student_npr_history_plot(map_mv,
                                  studentids = unique(z$studentid)[1:50],
                                  measurementscale = "Mathematics",
                                  title_text = "NPR Histories | KAP | 4th Grade | Math \ (first 90 students only)"
                                     )

al_p <- amys_lists(map_mv,
           studentids = unique(z$studentid),
           measurementscale = "Mathematics",
           start_fws = "Spring",
           start_academic_year = 2014,
           end_fws = "Spring",
           end_academic_year = 2015) +
  labs(title = "Growth Categories | KAP | 4th Grade | Math")


cgp_p <- cohort_cgp_hist_plot(map_mv,
                              studentids = unique(z$studentid),
                              measurementscale = "Mathematics")  +
  labs(title = "Class of 2024 | Avg RIT over times with cohort growth percentiles")

becca_p <- becca_plot(map_mv,studentids = z$studentid,measurementscale = "Mathematics",
           detail_academic_year = 2015) +
  labs(title = "Class of 2024 | Attainement by quartile over time")

scat_p <- scatter2(mapvizieR_obj = map_mv,
         studentids = unique(z$studentid),
         measurementscale = "Mathematics",
         start_fws = "Spring",
         start_academic_year = 2014,
         end_fws = "Spring",
         end_academic_year = 2015) +
  labs(title = "Student growth percentile vs percentile rank (attainment)\n 4th Grade | Spring 2015 - Spring 2016",
       x = "Growth Percentile\n(Spring 2015 - Spring 2016)",
       y = "National Percentile Rank\n(Spring 2016)")



# The Packet ####
the_packet <- function(school, abbrev, end_year, end_grade, subj){

  start_year <- end_year - 1
  start_grade <- end_grade - 1

  z <- map_mv$roster %>%
    filter(grepl(school, schoolname),
           map_year_academic == end_year,
           grade == end_grade)

  ids <- unique(z$studentid)

  ids_length <- length(ids)
  ids_half <- as.integer(ceiling(ids_length/2))

  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }

  cohort <- Mode(z$implicit_cohort)

  title_stub <- sprintf("%s | %s grade (Class of %s) | %s | Spring %s - Spring %s",
                        abbrev,
                        toOrdinal::toOrdinal(end_grade),
                        cohort,
                        subj,
                        start_year + 1,
                        end_year + 1)

  hp <- haid_plot(map_mv,
                  studentids = ids,
                  measurementscale = subj,
                  start_fws = "Spring",
                  start_academic_year = start_year,
                  end_fws = "Spring",
                  end_academic_year = end_year) +
    labs(title = "Waterfall (MAP Goals and Performance)",
         subtitle = title_stub)


  ids_1 <- ids[1:ids_half]
  ids_2 <- ids[(ids_half+1):ids_length]

  npr_p_1 <- student_npr_history_plot(map_mv,
                                    studentids =ids_1,
                                    measurementscale = subj,
                                    title_text = "NPR Histories"
  ) +
    labs(subtitle = sprintf("Page 1 | %s", title_stub))

  npr_p_2 <- student_npr_history_plot(map_mv,
                                      studentids =ids_2,
                                      measurementscale = subj,
                                      title_text = "NPR Histories"
  ) +
    labs(subtitle = sprintf("Page 2 | %s", title_stub))



  al_p <- amys_lists(map_mv,
                     studentids = ids,
                     measurementscale = subj,
                     start_fws = "Spring",
                     start_academic_year = start_year,
                     end_fws = "Spring",
                     end_academic_year = end_year) +
    labs(title = "Growth Categories",
         subtitle = title_stub)


  cgp_p <- cohort_cgp_hist_plot(map_mv,
                                studentids = ids,
                                measurementscale = subj)  +
    labs(title = "Avg RIT over times with cohort growth percentiles",
         subtitle = title_stub)


  becca_p <- becca_plot(map_mv,
                        studentids = ids,
                        measurementscale = subj,
                        detail_academic_year = end_year) +
    labs(title = "Attainement by quartile over time",
         subtitle = sprintf("Class of %s | %s",
                            cohort,
                            subj))

  scat_p <- scatter2(mapvizieR_obj = map_mv,
                     studentids = ids,
                     measurementscale = subj,
                     start_fws = "Spring",
                     start_academic_year = start_year,
                     end_fws = "Spring",
                     end_academic_year = end_year) +
    labs(title = "Student growth percentile vs percentile rank (attainment)",
         subtitle = title_stub,
          x = sprintf("Growth Percentile\n(Spring %s - Spring %s)", start_year+1, end_year+1),
          y=  sprintf("National Percentile Rank\n(Spring %s)", end_year+1))



  out <- list()

  out[["hp"]] <- hp
  out[["npr_p_1"]] <- npr_p_1
  out[["npr_p_2"]] <- npr_p_2
  out[["al_p"]] <- al_p
  out[["scat_p"]] <- scat_p
  out[["cgp_p"]] <- cgp_p
  out[["becca_p"]] <- becca_p


  out

}



grade_1_math_14 <- the_packet("Ascend Primary", "KAP", 2013, 1, "Mathematics")
grade_1_reading_14 <- the_packet("Ascend Primary", "KAP", 2013, 1, "Reading")
grade_2_math_14 <- the_packet("Ascend Primary", "KAP", 2013, 2, "Mathematics")
grade_2_reading_14 <- the_packet("Ascend Primary", "KAP", 2013, 2, "Reading")

grade_1_math_15 <- the_packet("Ascend Primary", "KAP", 2014, 1, "Mathematics")
grade_1_reading_15 <- the_packet("Ascend Primary", "KAP", 2014, 1, "Reading")
grade_2_math_15 <- the_packet("Ascend Primary", "KAP", 2014, 2, "Mathematics")
grade_2_reading_15 <- the_packet("Ascend Primary", "KAP", 2014, 2, "Reading")

grade_1_math_16 <- the_packet("Ascend Primary", "KAP", 2015, 1, "Mathematics")
grade_1_reading_16 <- the_packet("Ascend Primary", "KAP", 2015, 1, "Reading")
grade_2_math_16 <- the_packet("Ascend Primary", "KAP", 2015, 2, "Mathematics")
grade_2_reading_16 <- the_packet("Ascend Primary", "KAP", 2015, 2, "Reading")


cairo_pdf(file = "reports/kandersen_grades_1_2_14_15_16.pdf", height = 8.25, width = 10.75, onefile = TRUE)


grade_1_math_14
grade_1_reading_14
grade_2_math_14
grade_2_reading_14

grade_1_math_15
grade_1_reading_15
grade_2_math_15
grade_2_reading_15

grade_1_math_16
grade_1_reading_16
grade_2_math_16
grade_2_reading_16


dev.off()

