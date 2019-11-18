plot_tntp  <- function(data, domain_name, wrap_width = 30) {


  schools_data <-data %>%
    filter(!is.na(region),
           domain!="Sample Size",
           prompt != "Top Quartile",
           !grepl("Score", prompt),
           #!grepl("ffective", prompt),
           #!grepl("committed", prompt),
           !grepl("Match|match|CCRS|college", prompt),
           domain != "Instructional Culture Index",
           value <= 1.0001,
           !is.na(value),
           domain == domain_name
    ) %>%
    mutate(prompt2 = str_wrap(prompt, wrap_width),
           domain2 = str_wrap(domain, wrap_width)) %>%
    arrange(col_index) %>%
    mutate(prompt2 = forcats::fct_inorder(prompt2, ordered = TRUE))

  bm_data <-data %>%
    filter(!grepl("Grade Level", school),
           is.na(region),
           domain!="Sample Size",
           prompt != "Top Quartile",
           !grepl("Score", prompt),
           #!grepl("ffective", prompt),
           #!grepl("committed", prompt),
           !grepl("Match|match|CCRS|college", prompt),
           domain != "Instructional Culture Index",
           value <= 1.0001,
           !is.na(value),
           domain == domain_name
    ) %>%
    mutate(prompt2 = str_wrap(prompt, wrap_width),
           domain2 = str_wrap(domain, wrap_width)) %>%
    arrange(col_index) %>%
    mutate(prompt2 = forcats::fct_inorder(prompt2, ordered = TRUE))


  bm_kcs_avg <- bm_data %>%
    filter(str_detect(school, "Network Average")) %>%
    select(col_index, avg_network = value)

  bm_tq_avg <- bm_data %>%
    filter(str_detect(school, "Top Quartile")) %>%
    select(col_index, avg_tq = value)


  schools_data_2 <- schools_data %>%
    inner_join(bm_kcs_avg, by = "col_index") %>%
    inner_join(bm_tq_avg, by = "col_index") %>%
    mutate(gt_network_avg = value >= avg_network,
           gt_tq_avg = value >= avg_tq,
           fill_color = as.factor(gt_network_avg + gt_tq_avg))




  ggplot(schools_data_2) +
    geom_segment((aes(x=0, xend=value, y=school, yend=school, color=fill_color))) +
    geom_point(aes(x=value, y=school, color=fill_color)) +
    geom_vline(data = bm_data, aes(xintercept=value, alpha=school)) +
    facet_wrap(~ prompt2) +
    scale_x_continuous(labels = scales::percent) +
    #scale_color_viridis(direction = -1) +
    scale_color_manual(values = c(scales::muted("red"), "#BCD631","#439539"), guide = FALSE) +
    scale_alpha_discrete(range = c(.3,1)) +
    #theme_bw() +
    labs(x = "", y = "", alpha = "Benchmark", color = "% Agree")

}



plot_tntp_scores  <- function(data, school = "KAMS", wrap_width = 30) {


  ici <- "Instructional Culture Index"
  data <- data %>%
    mutate(
      prompt_1 = if_else(grepl(ici, prompt), ici, prompt),
      prompt_2 = str_replace(prompt_1, "\\d+$", ""),
      prompt = fct_inorder(prompt_2))

  schools_data <-data %>%
    filter(!is.na(region),
           domain!="Sample Size",
           prompt != "Top Quartile",
           grepl("Score|Index", prompt),
           #!grepl("Change", prompt),
           !grepl("ffective", prompt),
           !grepl("committed", prompt),
           !is.na(value)
    ) %>%
    mutate(prompt2 = str_wrap(prompt, wrap_width),
           domain2 = str_wrap(domain, wrap_width),
           school_intials = abbreviate(school, strict = FALSE, minlength = 3L))

  bm_data <-data %>%
    filter(!grepl("Grade Level", school),
           is.na(region),
           domain!="Sample Size",
           prompt != "Top Quartile",
           grepl("Score|Index", prompt),
           #!grepl("Change", prompt),
           !grepl("ffective", prompt),
           !grepl("committed", prompt),
           !is.na(value)) %>%
    mutate(prompt2 = str_wrap(prompt, wrap_width),
           domain2 = str_wrap(domain, wrap_width))


  ggplot(schools_data %>%
           mutate(school = str_wrap(school_intials, width=8)                  )
  ) +
    #geom_segment((aes(x=0, xend=value, y=school, yend=school, color=value))) +
    geom_bar(aes(y=value, x=school, fill=value), stat="identity") +
    geom_hline(data = bm_data, aes(yintercept=value, alpha=school)) +
    facet_wrap(~ prompt) +
    scale_fill_viridis(direction = -1) +
    scale_alpha_discrete(range = c(.3,1)) +
    theme_bw() +
    labs(x = "", y = "", alpha = "Benchmark", color = "Index Score")

}


#### Year over year
yoy <- function(school = "KAP", period_1 = tntp_f16, period_2 = tntp_s17) {

  period_1_name <- substitute(period_1) %>%
    as.character() %>%
    str_extract(regex("(f|s)\\d{2}$"))

  period_2_name <- substitute(period_2) %>%
    as.character() %>%
    str_extract(regex("(f|s)\\d{2}$"))

  extract_season <- function(x) if_else(str_detect(x, "f"), "Fall", "Spring")

  extract_year <- function(x) sprintf("20%s", str_extract(x, "\\d{2}"))


  season_year_1 <- sprintf("%s %s", extract_season(period_1_name), extract_year(period_1_name))
  season_year_2 <- sprintf("%s %s", extract_season(period_2_name), extract_year(period_2_name))

  tntp_yoy <- period_1 %>%
    mutate(period = season_year_1) %>%
    bind_rows(period_2 %>%
                mutate(period = season_year_2)) %>%
    mutate(period =  forcats::fct_inorder(period)) %>%
    filter(domain %in% domains) %>%
    mutate(
      abbrev = mapvizieR::abbrev(school,
                                 exceptions = list(old = "KAPS",
                                                   new = "KAP"))) %>%
    filter(!school %in% c("KIPP Network Average", "KIPP Top Quartile Schools")) %>%
    filter(stringr::str_detect(prompt,
                               "Score|Current Instructional Culture Index|compensated fairly|my workload|\\% of teachers planning to leave this year or next year"),
           !is.na(value)) %>%
    filter(!str_detect(prompt, "Effective")) %>%
    mutate(domain = forcats::fct_inorder(str_wrap(domain, width = 5)))


  #glimpse(tntp_yoy)

  tntp_yoy_2 <- tntp_yoy %>%
    mutate(period2 = if_else(period==season_year_1, "y0", "y1")) %>%
    select(abbrev, domain, period2, value) %>%
    tidyr::spread(period2, value) %>%
    mutate(diff = y1 - y0,
           positive = diff >= 0)

  ggplot(tntp_yoy_2 %>%
           filter(abbrev == school,
                  !is.na(diff)),
         aes()) +
    geom_segment(aes(x = 0, xend = 1, y=y0, yend=y1,
                     color = positive),
                 size = 3) +
    scale_color_manual(values = c(scales::muted("red"), "#439539"),
                       guide = FALSE) +
    scale_x_continuous(breaks = c(0,1), labels = c(toupper(period_2_name), toupper(period_2_name))) +
    #geom_text(aes(label=round(y0,1) ) )+
    facet_wrap(~domain) +
    theme_linedraw() +
    ylim(0, 10) +
    labs(x = sprintf("%s - %s", season_year_1, season_year_2),
         y = "Score")
}

read_tntp_school_sorter <- function(tntp_file) {
  #tntp_file <- "data/Fall 2017 KIPP Chicago_Teacher Survey School Sorter.xlsx"
  # grab headers and make tidy
  # headers<-read.xlsx(tntp_file,
  #                    sheetIndex = 2,
  #                         startRow = 1,
  #                         endRow = 2,
  #                         header = FALSE,
  #                         stringsAsFactors = FALSE)

  headers <- read_excel(tntp_file,
                        sheet =2,
                        range = cell_rows(1:2),
                        col_names = FALSE)



  headers_t <- headers %>%
    t() %>%
    as_tibble() %>%
    rename(domain = V1, prompt = V2) %>%
    mutate(col_index = sprintf("X__%s", rownames(.)),
           domain = zoo::na.locf(domain, na.rm=FALSE),
           prompt = ifelse(str_detect(prompt, "(School Sorter)|(School Summary)"), NA, prompt))
  # headers_t <- t(headers) %>%
  #   as.data.frame(stringsAsFactors = FALSE) %>%
  #   rename(domain = V1, prompt = V2) %>%
  #   mutate(col_index = rownames(.),
  #          domain = zoo::na.locf(domain, na.rm=FALSE),
  #          prompt = ifelse(str_detect(prompt, "School Sorter|School Summary"), NA, prompt))

  # grab column to use for informational purposes
  # column_A<-read.xlsx(tntp_file,
  #                     sheetIndex = 2,
  #                    colIndex = 1,
  #                    header = FALSE,
  #                    stringsAsFactors = FALSE)

  column_A <- read_excel(tntp_file,
                         sheet = 2,
                         range = cell_cols(1),
                         col_names = FALSE
  )

  # Get some info from column A like the first row with the word "Note"

  first_foundation_row <- grep("Insight Report|School Sorter", column_A$X__1) + 2
  note_row<-grep("Note", column_A$X__1) +1
  last_school_row <- note_row - 2
  last_foundation_row <- grep("^School$", column_A$X__1)


  # get foundation data
  # tntp_foundation <- read.xlsx(tntp_file,
  #                             sheetIndex = 2,
  #                    startRow = first_foundation_row,
  #                    endRow = first_foundation_row + 2,
  #                    header = FALSE,
  #                    stringsAsFactors = FALSE)

  tntp_foundation <- read_excel(tntp_file,
                                sheet = 2,
                                range = cell_rows(first_foundation_row:last_foundation_row),
                                col_names = FALSE)


  tntp_foundation <- tntp_foundation %>%
    dplyr::filter(!str_detect(X__1, "Below")) %>%
    rename(school = X__1,
           region = X__2,
           grade_level = X__3) %>%
    gather(col_index, value, 4:ncol(tntp_foundation)) %>%
    left_join(headers_t, by = "col_index")


  # get schools  data
  # tntp_schools <-read.xlsx(tntp_file,
  #                          sheetIndex = 2,
  #                             startRow = last_foundation_row + 2,
  #                             endRow = last_school_row,
  #                             header = FALSE,
  #                             stringsAsFactors = FALSE)

  tntp_schools <- read_excel(tntp_file,
                             sheet = 2,
                             range = cell_rows((last_foundation_row + 2):last_school_row),
                             col_names = FALSE)

  tntp_schools <- tntp_schools %>%
    rename(school = X__1,
           grade_level = X__2,
           region = X__3) %>%
    gather(col_index, value, 4:ncol(tntp_schools)) %>%
    left_join(headers_t, by = "col_index") %>%
    mutate(value = as.numeric(ifelse(value=="Top-Q", 1, value)))


  # combine schools and foundation data into one data frame

  tntp_f17 <- bind_rows(tntp_schools, tntp_foundation)
}





