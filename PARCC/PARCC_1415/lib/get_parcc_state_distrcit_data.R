require(dplyr)

# 1. Create scaffold on which to graft scores.

scaffold_parcc_ela <- function(
                           grade = 6,
                           year= "2014-2015",
                           level="PARCC"){

  # define peformacne levels
  perf_levels <- c("Below", "Nearly Meets", "Meets or Exceeds")

  # define reading content areas
  reading_scaffold <-
    dplyr::data_frame(
      sub_strand = c(NA,
                     rep("Literary", 3),
                     rep("Information", 3),
                     rep("Vocabulary", 3)),
      score_type = c("Score", rep(perf_levels, 3))
    ) %>%
    dplyr::mutate(subject = "ELA",
           strand = "Reading")

  writing_scaffold <-
    dplyr::data_frame(
      sub_strand = c(NA,
                     rep("Expression", 3),
                     rep("Conventions", 3)),
    score_type =c("Score",  rep(perf_levels, 2))) %>%
    dplyr::mutate(subject = "ELA",
           strand = "Writing")

  overall_scaffold <-
    dplyr::data_frame(subject = "ELA",
               strand = NA,
               score_type = "Overall Score"
               )

  dplyr::bind_rows(overall_scaffold, reading_scaffold, writing_scaffold) %>%
    dplyr::mutate(grade = grade, sy = year, level=level) %>%
    dplyr::select(sy, grade, level, subject, strand, sub_strand, score_type)

}


# math
scaffold_parcc_math <- function(
  grade = 6,
  year= "2014-2015",
  level="PARCC"){

  # define peformacne levels
  perf_levels <- c("Below", "Nearly Meets", "Meets or Exceeds")

  # define reading content areas
  math_scaffold <-
    dplyr::data_frame(
      sub_strand = c(
                     rep("Major Content", 3),
                     rep("Supporting Content", 3),
                     rep("Reasoning", 3),
                     rep("Modeling", 3)),
      score_type = rep(perf_levels, 4)
    ) %>%
    dplyr::mutate(subject = "Mathematics",
           strand = "Mathematics")

  overall_scaffold <-
    dplyr::data_frame(subject = "Mathematics",
               strand = NA,
               score_type = "Overall Score"
    )

  dplyr::bind_rows(overall_scaffold, math_scaffold) %>%
    dplyr::mutate(grade = grade, sy = year, level=level) %>%
    dplyr::select(sy, grade, level, subject, strand, sub_strand, score_type)

}


# function to parse reading scores. Scores (so far) appear
# across two lines, but differe on which data points are which
# line by grade.
line_parser_reading <- function(data, level = "PARCC", grade=5){

  #  ID row with data
  level_id <- stringr::str_to_upper(sprintf("%s AVERAGE", level))

  parcc_location <- suppressWarnings(grep(level_id, data))

  # Extract Row(s) Reading has the Literary percentages
  # offset by two rows.
  x_1 <- data[parcc_location]
  x_2 <- data[parcc_location+2]

  # Extract integers
  x_1_data<-x_1 %>%
    stringr::str_extract_all("\\d{2,3}") %>%
    purrr::flatten() %>%
    as.integer

  x_2_data <- x_2 %>%
    stringr::str_extract_all("\\d{2,3}") %>%
    purrr::flatten() %>%
    as.integer

  if(grade %in% c(6,8)){
    # Insert Literary data into other data
    x_3 <- insert_into(x_1_data, x_2_data, 2)
  }
  if(grade %in% c(3,4,5,7)){
    x_3 <- c(x_1_data[1:2],
             x_2_data[1:9],
             x_1_data[3],
             x_2_data[10:length(x_2_data)]
             )
  }

  #return
  x_3

}

# math
line_parser_math <- function(data, level = "PARCC", grade=5){

  #  ID row with data
  level_id <- stringr::str_to_upper(sprintf("%s AVERAGE", level))

  parcc_location <- suppressWarnings(grep(level_id, data))

  # Extract Row(s) Reading has the Literary percentages
  # offset by two rows.
  x_1 <- data[parcc_location]
  x_2 <- data[parcc_location+2]

  # Extract integers
  x_1_data<-x_1 %>%
    stringr::str_extract_all("\\d{2,3}") %>%
    purrr::flatten() %>%
    as.integer

  x_2_data <- x_2 %>%
    stringr::str_extract_all("\\d{2,3}") %>%
    purrr::flatten() %>%
    as.integer

  if(grade %in% c(3:8)){
    # Insert Literary data into other data
    x_3 <- c(x_1_data, x_2_data)
  }

  #return
  x_3

}

# quick function to splice one vector into another
# at after a given insertion point
insert_into <- function(x1, x2, insert_point) {

  end <- length(x1)
  start <- insert_point + 1
  c(x1[1:insert_point], x2, x1[start:end])

}


# Function to exctract a row form student roster pdf
# data is the outpute of tm::readPDF functional
extract_parcc_bechmarks_rows <- function(data,
                                    subject = "ELA",
                                    year = "2014-2015",
                                    level = "PARCC",
                                    grade = 6) {

  #2. erect scaffold
  if(subject == "ELA") {
    test_scaffold <- scaffold_parcc_ela(
                                    grade = grade,
                                    year = year,
                                    level = level)

    scores <- line_parser_reading(data = data,
                                  level = level,
                                  grade = grade)


    # Add scores to scaffold and
    # return
    out<-test_scaffold %>%
      dplyr::mutate(score = scores)
  }

  if(subject == "Mathematics") {
    test_scaffold <- scaffold_parcc_math(
                                    grade = grade,
                                    year = year,
                                    level = level)

    scores <- line_parser_math(data = data,
                                  level = level,
                                  grade = grade)


    # Add scores to scaffold and
    # return
    out <- test_scaffold %>%
      dplyr::mutate(score = scores)
  }


  # return
  out

}


# wrapper to run extract_parcc_bechmarks_rows over all three levels

extract_parcc_bechmarks <-  function(data,
                                     subject = "ELA",
                                     year = "2014-2015",
                                     grade = 6) {

  c("PARCC", "State", "District") %>%
    purrr::map_df(~extract_parcc_bechmarks_rows(data=data,
                                         level = .x,
                                         subject = subject,
                                         year = year,
                                         grade = grade))


}



