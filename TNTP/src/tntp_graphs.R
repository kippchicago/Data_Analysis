require(stringr)
require(purrr)
require(viridis)
require(forcats)


plot_tntp  <- function(data, domain_name, wrap_width = 30) {


  schools_data <-data %>%
    filter(!is.na(region),
           domain!="Sample Size",
           prompt != "Top Quartile",
           !grepl("Score", prompt),
           !grepl("ffective", prompt),
           !grepl("committed", prompt),
           !grepl("Match|match|CCRS|college", prompt),
           domain != "Instructional Culture Index",
           value <= 1.0001,
           !is.na(value),
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
           !grepl("committed", prompt),
           !grepl("Match|match|CCRS|college", prompt),
           domain != "Instructional Culture Index",
           value <= 1.0001,
           !is.na(value),
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

plot_tntp(tntp, domains[3], 30)


domains %>%


tntp %>%
  filter(!is.na(region),
         domain!="Sample Size",
         prompt != "Top Quartile",
         grepl("Score|Index", prompt),
         #!grepl("Change", prompt),
         !grepl("ffective", prompt),
         !grepl("committed", prompt),
         value >= 1) %>% as.data.frame()



# plot index data ####

plot_tntp_scores  <- function(data, wrap_width = 30) {


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
    filter(is.na(region),
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



domains[-1]

x <- list()
x <-
  domains[-1] %>%
  map(~plot_tntp(tntp, .) + ggtitle(.))



tntp %>% glimpse

domains

tntp_domains <- tntp %>%
  filter(domain %in% domains) %>%
  mutate(
         abbrev = mapvizieR::abbrev(school,
                                    exceptions = list(old = "KAPS",
                                                      new = "KAP")))


tntp_domains$school %>% unique()

tntp_domains_kipp_tq <- tntp_domains %>% filter(school == "KIPP Top Quartile Schools")
tntp_domains_kipp_avg <- tntp_domains %>% filter(school == "KIPP Network Average")
tntp_domains_kipp_kcs <- tntp_domains %>% filter(!school %in% c("KIPP Network Average", "KIPP Top Quartile Schools"))


# Need to redeuce to single score for each domeain then inner_join by col index ####
tntp_tile_data <- tntp_domains_kipp_kcs %>%
  filter(stringr::str_detect(prompt,
                             "Score|Current Instructional Culture Index|compensated fairly|my workload|\\% of teachers planning to leave this year or next year"),
         !is.na(value)) %>%
  filter(!str_detect(prompt, "Effective")) %>%
  inner_join(tntp_domains_kipp_tq %>%
               select(col_index,
                      value_tq_avg = value),
             by = "col_index") %>%
  inner_join(tntp_domains_kipp_avg %>%
               select(col_index,
                      value_kn_avg = value),
             by = "col_index") %>%
  mutate(score = (value>=value_kn_avg) + (value>=value_tq_avg),
         domain = forcats::fct_inorder(str_pad(str_wrap(domain, width = 10), side = "both", width=1)),
         schools2 = if_else(abbrev == "KC", "Region", abbrev),
         schools3 = forcats::fct_relevel(schools2, "Region", "KAP", "KAMS", "KCCP", "KBCP", "KOP", "KOA"))

ggplot(tntp_tile_data, aes(x=as.factor(0), y=as.factor(0))) +
  geom_tile(aes(fill = as.factor(score)), color = "white", size = 2) +
  facet_grid(schools3 ~ domain, switch="y") +
  theme_linedraw() +
  theme(axis.text =  element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.text.x = element_text(size = 10),
        strip.text.y = element_text(angle = 180,
                                    size = 10),
        legend.position = "bottom") +
  scale_fill_manual("KIPP Chicago score is:", values = c(scales::muted("red"), "#BCD631","#439539"),
                    labels = c("< Network Avg", "≥ Network Avg", "≥ Top Quartile Avg "))


# Quickly look at KCCP and KAP for year over year

tntp_yoy <- tntp_S16 %>%
  mutate(period = "Spring 2016") %>%
  bind_rows(tntp %>%
              mutate(period = "Fall 2016")) %>%
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


glimpse(tntp_yoy)

tntp_yoy_2 <- tntp_yoy %>%
  mutate(period2 = if_else(period=='Spring 2016', "y0", "y1")) %>%
  select(abbrev, domain, period2, value) %>%
  tidyr::spread(period2, value) %>%
  mutate(diff = y1 - y0,
         positive = diff >= 0)

ggplot(tntp_yoy_2 %>%
         filter(abbrev == "KAP",
                !is.na(diff)),
       aes()) +
  geom_segment(aes(x = 0, xend = 1, y=y0, yend=y1,
                   color = positive),
               size = 3) +
  scale_color_manual(values = c(scales::muted("red"), "#439539"),
                     guide = FALSE) +
  scale_x_continuous(breaks = c(0,1), labels = c("S", "F")) +
  #geom_text(aes(label=round(y0,1) ) )+
  facet_wrap(~domain) +
  theme_linedraw() +
  ylim(0, 10) +
  labs(x = "Spring and Fall 2016",
       y = "Score")



# Overall Scores for kap highlighted

plot_tntp_scores(tntp)

+
  theme(axis.text.x = element_text(color = c("black", "red", "black", "black")))
