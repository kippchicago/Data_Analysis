require(stringr)
require(purrr)
require(viridis)


stringr::str_wrap()


plot_tntp  <- function(data, domain_name, wrap_width = 30) {


  schools_data <-data %>%
    filter(!is.na(region),
           domain!="Sample Size",
           prompt != "Top Quartile",
           !grepl("Score", prompt),
           !grepl("ffective", prompt),
           domain != "Instructional Culture Index",
           value <= 1.0001,
           domain == domain_name
    ) %>%
    mutate(prompt2 = str_wrap(prompt, wrap_width),
           domain2 = str_wrap(domain, wrap_width))

  bm_data <-data %>%
    filter(is.na(region),
           domain!="Sample Size",
           prompt != "Top Quartile",
           !grepl("Score", prompt),
           !grepl("ffective", prompt),
           domain != "Instructional Culture Index",
           value <= 1.0001,
           domain == domain_name
    ) %>%
    mutate(prompt2 = str_wrap(prompt, wrap_width),
           domain2 = str_wrap(domain, wrap_width))


  ggplot(schools_data) +
    geom_segment((aes(x=0, xend=value, y=school, yend=school, color=value))) +
    geom_point(aes(x=value, y=school, color=value)) +
    geom_vline(data = bm_data, aes(xintercept=value, alpha=school)) +
    facet_wrap(~ prompt2) +
    scale_x_continuous(labels = scales::percent) +
    scale_color_viridis(direction = -1) +
    scale_alpha_discrete(range = c(.3,1)) +
    theme_bw() +
    labs(x = "", y = "", alpha = "Benchmark", color = "% Agree")

}


domains <- unique(tntp$domain)[-1]

plot_tntp(tnpt, domains[3], 30)

tnpt %>%
  filter(!is.na(region),
         domain!="Sample Size",
         prompt != "Top Quartile",
         grepl("Score|Index", prompt),
         !grepl("Change", prompt),
         !grepl("ffective", prompt),
         value >= 1) %>% as.data.frame()



# plot index data ####

plot_tntp_scores  <- function(data, wrap_width = 30) {


  schools_data <-data %>%
    filter(!is.na(region),
           domain!="Sample Size",
           prompt != "Top Quartile",
           grepl("Score|Index", prompt),
           !grepl("Change", prompt),
           !grepl("ffective", prompt),
           value >= 1
           ) %>%
    mutate(prompt2 = str_wrap(prompt, wrap_width),
           domain2 = str_wrap(domain, wrap_width))

  bm_data <-data %>%
    filter(is.na(region),
            domain!="Sample Size",
            prompt != "Top Quartile",
            grepl("Score|Index", prompt),
            !grepl("Change", prompt),
            !grepl("ffective", prompt),
            value >= 1) %>%
    mutate(prompt2 = str_wrap(prompt, wrap_width),
           domain2 = str_wrap(domain, wrap_width))


  ggplot(schools_data %>%
           mutate(school = str_wrap(school, width=8))) +
    #geom_segment((aes(x=0, xend=value, y=school, yend=school, color=value))) +
    geom_bar(aes(y=value, x=school, fill=value), stat="identity") +
    geom_hline(data = bm_data, aes(yintercept=value, alpha=school)) +
    facet_wrap(~ prompt) +
    scale_fill_viridis(direction = -1) +
    scale_alpha_discrete(range = c(.3,1)) +
    theme_bw() +
    labs(x = "", y = "", alpha = "Benchmark", color = "Index Score")

}









domains

x <- list()
x <-
  domains %>%
  map(~plot_tntp(tnpt, .))

