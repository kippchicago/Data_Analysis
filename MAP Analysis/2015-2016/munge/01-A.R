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

report_school <- "Create"

# map_all_silo <- map_all_silo %>%
#   mutate(SchoolName = ifelse(grepl("Bloom", SchoolName) &
#                                Grade == 5,
#                              "KIPP Academy South",
#                              SchoolName)
#   )

map_sep <- separate_cdf(map_all_silo, district_name = "KIPP Chicago")

map_sep$roster <- map_sep$roster %>%
  left_join(current_ps_roster %>%
              select(StudentID, Home_Room),
            by="StudentID") %>%
  mutate(current_student = !is.na(Home_Room))


map_sep$cdf <- map_sep$cdf %>%
  mutate(school_initials = abbrev(SchoolName,
                                  exceptions = list(old = "KAPS", new = "KAP")
  ),
  school_initials = factor(school_initials,
                           levels = c("KAP", "KAMS", "KCCP", "KBCP", "KAS"))
  )

map_viz<-mapvizieR(map_sep$cdf, map_sep$roster, include_unsanctioned_windows = TRUE)

map_viz_2011_norms<-mapvizieR(map_sep$cdf, map_sep$roster,
                              include_unsanctioned_windows = TRUE,
                              norm_df_long = mapvizieR:::norms_students_wide_to_long(student_growth_norms_2011))

map_viz_2015_norms<-mapvizieR(map_sep$cdf, map_sep$roster,
                              include_unsanctioned_windows = TRUE,
                              norm_df_long = mapvizieR:::norms_students_wide_to_long(student_growth_norms_2015))


ProjectTemplate::cache('map_viz')
