# CPS Comparison for KIPP RC

setwd("~/Dropbox (KIPP Chicago Schools)/Data Analysis/MAP Analysis/2014-2015")

require(readr)
require(dplyr)
require(tidyr)


cps_attain <- readr::read_csv("data//NWEAreportSchool_2014_Attain.csv")

cps_attain2<-cps_attain %>% 
  select(-X:-X.6) %>% #drop extraneous columsn
  gather(variable, value, X2013_N:X2014_School_Attainment_Pctl) %>%
  mutate(variable = gsub("X", "", variable),
         variable = gsub("(\\d+)_", "\\1@", variable)) %>%
  separate(variable, c("year", "variable"), "@") %>%
  mutate(year=as.integer(year),
         KIPP=grepl("KIPP ", SchoolName),
         variable=ifelse(variable=="50th_Pctl", "Pctl_50th", variable)
         ) 

cps_pct_50<-cps_attain2 %>% 
  filter(variable %in% c("N", "Pctl_50th"),
         year==2014
         ) %>%
  spread(variable, value) %>%
  mutate(Pctl_x_N=Pctl_50th*N) %>%
  group_by(Subject, Grade) %>%
  dplyr::summarize(N=sum(N, na.rm = TRUE),
                   Pclt_sum=sum(Pctl_x_N, na.rm = TRUE),
                   Pct_at_50th=sum(Pclt_sum)/N
                   ) %>%
  mutate(Type="CPS",
         School="All")

kcs_pct_50<-cps_attain2 %>% 
  filter(variable %in% c("N", "Pctl_50th"),
         year==2014,
         KIPP
  ) %>%
  spread(variable, value) %>%
  mutate(Pctl_x_N=Pctl_50th*N) %>%
  group_by(Subject, Grade) %>%
  dplyr::summarize(N=sum(N, na.rm = TRUE),
                   Pclt_sum=sum(Pctl_x_N, na.rm = TRUE),
                   Pct_at_50th=sum(Pclt_sum)/N
  ) %>%
  mutate(Type="KIPP",
         School="All")


kacp_pct_50<-cps_attain2 %>% 
  filter(variable %in% c("N", "Pctl_50th"),
         year==2014,
         KIPP,
         grepl("ASCEND", SchoolName)
  ) %>%
  spread(variable, value) %>%
  mutate(Pctl_x_N=Pctl_50th*N) %>%
  group_by(Subject, Grade) %>%
  dplyr::summarize(N=sum(N, na.rm = TRUE),
                   Pclt_sum=sum(Pctl_x_N, na.rm = TRUE),
                   Pct_at_50th=sum(Pclt_sum)/N
  ) %>%
  mutate(Type="KIPP",
         School="KIPP Ascend")

kccp_pct_50<-cps_attain2 %>% 
  filter(variable %in% c("N", "Pctl_50th"),
         year==2014,
         KIPP,
         grepl("CREATE", SchoolName)
  ) %>%
  spread(variable, value) %>%
  mutate(Pctl_x_N=Pctl_50th*N) %>%
  group_by(Subject, Grade) %>%
  dplyr::summarize(N=sum(N, na.rm = TRUE),
                   Pclt_sum=sum(Pctl_x_N, na.rm = TRUE),
                   Pct_at_50th=sum(Pclt_sum)/N
  ) %>%
  mutate(Type="KIPP",
         School="KIPP Create")

kbcp_pct_50<-cps_attain2 %>% 
  filter(variable %in% c("N", "Pctl_50th"),
         year==2014,
         KIPP,
         grepl("BLOOM", SchoolName)
  ) %>%
  spread(variable, value) %>%
  mutate(Pctl_x_N=Pctl_50th*N) %>%
  group_by(Subject, Grade) %>%
  dplyr::summarize(N=sum(N, na.rm = TRUE),
                   Pclt_sum=sum(Pctl_x_N, na.rm = TRUE),
                   Pct_at_50th=sum(Pclt_sum)/N
  ) %>%
  mutate(Type="KIPP",
         School="KIPP Bloom")

  
combined_pct_50 <- rbind_list(kcs_pct_50, 
                              cps_pct_50,
                              kbcp_pct_50,
                              kccp_pct_50,
                              kacp_pct_50) %>% 
  mutate(Type=factor(Type, levels = c("KIPP", "CPS"))) %>%
  as.data.frame


# KCS ####
p_all<-ggplot(combined_pct_50 %>% 
         filter(!grepl("Combined|4", Grade),
                School=="All"), 
       aes(x=Grade, y=Pct_at_50th)) +
  geom_bar(aes(fill=Type), stat = "identity",
           color=NA,
           position="dodge") + 
  scale_fill_manual(values = c("#60A2D7",
                               "#CFCCC1")) + 
  facet_grid(.~Subject) +
  theme_bw() +
  ylab("Percent of students at or above\ngrade level") + 
  xlab("") +
  ggtitle("Percent of students at or above grade level")



# KACP ####
p_kacp<-ggplot(combined_pct_50 %>% 
                filter(!grepl("Combined|4", Grade),
                       (School=="All"&Type=="CPS")|
                         School=="KIPP Ascend"),
              aes(x=Grade, y=Pct_at_50th)) +
  geom_bar(aes(fill=Type), stat = "identity",
           color=NA,
           position="dodge") + 
  scale_fill_manual(values = c("#60A2D7",
                               "#CFCCC1")) + 
  facet_grid(.~Subject) +
  theme_bw() +
  ylab("Percent of students at or above\ngrade level") + 
  xlab("") +
  ggtitle("Percent of students at or above grade level")

# KCCP ####
p_kccp<-ggplot(combined_pct_50 %>% 
                 filter(!grepl("Combined", Grade),
                        Grade %in% c("Grade 5", "Grade 6"),
                        (School=="All"&Type=="CPS")|
                          School=="KIPP Create"),
               aes(x=Grade, y=Pct_at_50th)) +
  geom_bar(aes(fill=Type), stat = "identity",
           color=NA,
           position="dodge") + 
  scale_fill_manual(values = c("#60A2D7",
                               "#CFCCC1")) + 
  facet_grid(.~Subject) +
  theme_bw() +
  ylab("Percent of students at or above\ngrade level") + 
  xlab("") +
  ggtitle("Percent of students at or above grade level")


# KCCP ####
p_kbcp<-ggplot(combined_pct_50 %>% 
                 filter(!grepl("Combined", Grade),
                        Grade %in% c("Grade 5"),
                        (School=="All"&Type=="CPS")|
                          School=="KIPP Bloom"),
               aes(x=Grade, y=Pct_at_50th)) +
  geom_bar(aes(fill=Type), stat = "identity",
           color=NA,
           position="dodge") + 
  scale_fill_manual(values = c("#60A2D7",
                               "#CFCCC1")) + 
  facet_grid(.~Subject) +
  theme_bw() +
  ylab("Percent of students at or above\ngrade level") + 
  xlab("") +
  ggtitle("Percent of students at or above grade level")

  
