

# connect to Illuminate mirror
ill<-connect_illuminate(server = "54.172.11.151",
                        database = "Illuminate_mirror",
                        user = "silo",
                        password = "silKIPP1",
                        db_type = "warehouse")



ps<-connect_illuminate(server = "54.172.11.151",
                        database = "PS_mirror",
                        user = "silo",
                        password = "silKIPP1",
                        db_type = "warehouse")

ill_obj <- illuminater(ill, ps, "students")

# add school initials to roster file

school_ids_names <- data_frame(schoolid = c(78102, 7810, 400146, 400163),
                               school = c("KAP", "KAMS", "KCCP", "KBCP")
                               )


ill_obj$roster <- ill_obj$roster %>%
  inner_join(school_ids_names, by="schoolid") %>%
  mutate(school = ifelse(school == "KBCP" & grade_level == 5,
                         "KAS",
                         school))


p <- mastery_grid_plot(ill_obj$results,
                       ill_obj$roster %>% filter(home_room == "6th NIU"),
                       school_id = 7810,
                       school_name = "KAMS",
                       academic_year==2016,
                       assm_grade==6,
                       assm_subj == "math",
                       assm_unit == "u1")

# Unit 1 math assessments at Bloom in 7th grade


hrs_7_kbcp <- ill_obj$roster %>%
  filter(enroll_status == 0,
         schoolid == 400163,
         grade_level == 7) %>%
  select(home_room) %>%
  unique()


grid_plot_list <- list()
for (hr in hrs_7_kbcp$home_room) {
  grid_plot_list[[hr]] <- mastery_grid_plot(ill_obj$results,
                                            ill_obj$roster %>% filter(home_room == hr),
                                            school_id = 400163,
                                            school_name = "KBCP",
                                            academic_year==2016,
                                            assm_grade==7,
                                            assm_subj == "math",
                                            assm_unit == "u1") +
    ggtitle(sprintf("KBCP 7th Math: %s", hr))

}

grid_plot_list$`7th Georgetown`

cairo_pdf(filename = "~/Dropbox (KIPP Chicago Schools)/Data Analysis/Illuminate/Data_Reporting_Prototypes/graphs/master_grid_prototype.pdf",
          width = 10.8,
          height = 8.3,
          onefile = TRUE
          )

  grid_plot_list

dev.off()

require(purrr)
require(toOrdinal)

ordinate <- function(x) {
  if(is.numeric(x)) {
    if(!is.integer(x)) x <- as.integer(x)
    if(x>0) toOrdinal::toOrdinal(x)
  } else {
     x
  }
}


all_grid_plots <- function(ill_obj, subj, unit){

  require(dplyr)
  require(purrr)
  require(ggplot2)

  mastery_grid_plots <- ill_obj$roster %>%
    filter(enroll_status == 0) %>%
    split(list(.$school, .$grade_level, .$home_room)) %>%
    discard(~nrow(.)==0) %>%
    map( ~ try(mastery_grid_plot(ill_obj$results,
                                 .,
                                 school_id = unique(.$schoolid),
                                 school_name = as.character(unique(.$school)),
                                 academic_year==2016,
                                 assm_grade==unique(.$grade_level),
                                 assm_subj == subj,
                                 assm_unit == unit) +
                 ggtitle(sprintf("%s %s %s: %s",
                                 ordinate(unique(.$grade_level)),
                                 unique(.$school),
                                 toupper(subj),
                                 unique(.$home_room)
                 )
                 ),
               silent = TRUE
    )
    ) %>%
    discard(~inherits(., "try-error"))

  # return
  mastery_grid_plots
}


all_long_plots <- function(ill_obj, subj, unit) {

  require(dplyr)
  require(purrr)
  require(ggplot2)

  mastery_long_plots <- ill_obj$roster %>%
    filter(enroll_status == 0) %>%
    split(list(.$school, .$grade_level)) %>%
    discard(~nrow(.)==0) %>%
    map( ~ try(mastery_long_plot(ill_obj$results,
                                 .,
                                 school_id = unique(.$schoolid),
                                 school_name = as.character(unique(.$school)),
                                 academic_year==2016,
                                 assm_grade==unique(.$grade_level),
                                 assm_subj == subj,
                                 assm_unit == unit) +
                 ggtitle(sprintf("%s  %s: %s",
                                 ordinate(unique(.$grade_level)),
                                 toupper(subj),
                                 unique(.$school)
                 )
                 ),
               silent = TRUE
    )
    ) %>%
    discard(~inherits(., "try-error"))

  mastery_long_plots
}



todays_date <- format(lubridate::today(), "%y%m%d")

cairo_pdf(sprintf("~/Dropbox (KIPP Chicago Schools)/Data Analysis/Illuminate/Data_Reporting_Prototypes/graphs/mastery_summaries_math_sci_%s.pdf",
                  todays_date),
          width=11, height=8.5, onefile=TRUE)

  all_long_plots(ill_obj, "math", "u1")
  all_long_plots(ill_obj, "sci", "u2")

dev.off()


cairo_pdf(sprintf("~/Dropbox (KIPP Chicago Schools)/Data Analysis/Illuminate/Data_Reporting_Prototypes/graphs/mastery_grids_math_sci_%s.pdf",
                  todays_date),
          width=11, height=8.5, onefile=TRUE)

  all_grid_plots(ill_obj, "math", "u1")
  all_grid_plots(ill_obj, "sci", "u2")

dev.off()
