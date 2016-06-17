
project_s2s<-function(.data, student_column = "studentid",
                      grade_column = "grade",
                      subject_column = "measurementscale",
                      rit_column = "testritscore",
                      term_column = "termname",
                      school_indicator = "schoolname",
                      norms = 2015) {

  #select only necessary columns
  map_df<-.data %>%
    dplyr::rename_(studentid=student_column,
                   grade=grade_column,
                   measurementscale=subject_column,
                   testritscore=rit_column,
                   termname=term_column,
                   school=school_indicator
    )


  df_length<-nrow(map_df)
  map_df_s2 <- map_df %>%
    dplyr::inner_join(norms_students_2011 %>%
                        dplyr::select(measurementscale=MeasurementScale,
                                      grade=StartGrade,
                                      testritscore=StartRIT,
                                      typical_growth=T22,
                                      reported_growth=R22,
                                      sd_growth=S22
                        ) %>%
                        mutate(grade=as.numeric(grade)),
                      by=c("measurementscale",
                           "grade",
                           "testritscore")
    ) %>%
    dplyr::mutate(map_year_academic=map_year_academic+1,
                  termname=paste0(fallwinterspring,
                                  " ",
                                  map_year_academic,
                                  "-",
                                  map_year_academic+1),
                  testquartile=mapvizieR::kipp_quartile(testpercentile),
                  tiered_factor=mapvizieR::tiered_growth_factors(testquartile, grade),
                  tiered_growth=round(reported_growth*tiered_factor),
                  simulated_growth=round(rnorm(n(), typical_growth, sd_growth)),
                  testritscore_tiered=testritscore+tiered_growth,
                  testritscore_nontiered=testritscore+simulated_growth
    ) %>%
    dplyr:: group_by(school, grade) %>%
    dplyr:: mutate(cr_projected=as.logical(rbinom(n(),1,prob = percent_cr)),
                   testritscore=ifelse(cr_projected, testritscore_tiered, testritscore_nontiered)
    ) %>%
    ungroup %>%
    dplyr::mutate(grade=grade+1,
                  testpercentile=ifelse(cr_projected, sqrpr::get_test_percentile(measurementscale,
                                                                                 fallwinterspring,
                                                                                 grade,
                                                                                 testritscore),
                                        testpercentile)
    )  %>%
    select(-testquartile,
           -tiered_factor,
           -tiered_growth,
           -simulated_growth,
           -testritscore_tiered,
           -testritscore_nontiered,
           -typical_growth,
           -reported_growth,
           -sd_growth
    )

  out<-dplyr::rbind_list(map_df, map_df_s2) %>%
    rename(schoolname=school)

  out
}


# seperate combined cdf to assessment results and students by school
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
