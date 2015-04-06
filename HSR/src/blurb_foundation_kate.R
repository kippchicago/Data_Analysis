# 2013-14 MAP performance by region ####

# read data
map.network<-read.csv('data/Historical_MAP_2014 07 21.csv')
map.network<-mutate(map.network, 
                    met_typical=as.integer(gsub("%", "", met_typical)),
                    met_tiered=as.integer(gsub("%", "", met_tiered))
)


map_6th <- map.network %>% filter(Growth_Grade_Level==6, 
                                  Growth_Academic_Year==2014,
                                  Sub_Test_Name %in% 
                                    c("Mathematics", "Reading"))

ranked_season_subject<-map_6th %>%
  group_by(Start_Season, Sub_Test_Name) %>%
  mutate(rank_typical=row_number(-met_typical),
         rank_tiered=row_number(-met_tiered),
         rank_mag=row_number(-magnitude),
         N=n()) %>%
  select(School_Display_Name, Start_Season, Sub_Test_Name, N,
         rank_typical,  
         met_typical,
         rank_tiered,
         met_tiered,
         rank_mag, 
         magnitude) %>%
  arrange(rank_typical)

ranked_season_subject %>% 
  filter(grepl("Create", School_Display_Name))

# Typical ####

ranked_season_subject %>% filter(rank_typical<=6) %>%
  group_by(School_Display_Name,  Start_Season) %>%
  summarize(Total=n()) %>% ungroup %>%
  arrange(desc(Total))

# Create is one of only two schools in the top 6 for Fall for
# both math and reading fall to spring % met typical

# Tiered ####

ranked_season_subject %>% filter(rank_tiered<=7) %>%
  group_by(School_Display_Name,  Start_Season) %>%
  summarize(Total=n()) %>% ungroup %>%
  arrange(desc(Total))

# Create is one of only **three** schools in the top 7 for Fall for
# both math and reading fall to spring % met tired

# magnitude ####

ranked_season_subject %>% filter(rank_mag<=4) %>%
  group_by(School_Display_Name,  Start_Season) %>%
  summarize(Total=n()) %>% ungroup %>%
  arrange(desc(Total))

# Create is the ***only schools*** in the top 4 for Fall for
# both math and reading fall to spring % met tired

# Typical and tiered combined

ranked_season_subjects_combined<-map_6th %>% 
  group_by(School_Display_Name, Start_Season) %>%
  summarize(avg_typical=weighted.mean(met_typical, n_count),
            avg_tiered=weighted.mean(met_tiered, n_count)) %>%
  group_by(Start_Season) %>%
  mutate(rank_typical=row_number(-avg_typical),
         rank_tiered=row_number(-avg_tiered),
         N=n())


ranked_season_subjects_combined %>% ungroup %>%
  arrange(rank_typical) %>%
  filter(grepl("Create", School_Display_Name))

# When combined KIPP Ranks 2nd for meeting typical growth
# adn 4th for making tiered growth.  All this is for 
# fall to spring 


