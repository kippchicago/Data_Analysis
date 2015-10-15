# HSR analyis for Aug 2015 Board meeting

# Read HSR data ####


require(readr)

hsr_list <-list()
hsr_list$teachers<-read_csv("../../../HSR/data/HSR_schools_teachers_1415.csv")
hsr_list$staff<-read_csv("../../../HSR/data/HSR_schools_staff_1415.csv")
hsr_list$students<-read_csv("../../../HSR/data/HSR_schools_students_1415.csv")
hsr_list$parents<-read_csv("../../../HSR/data/HSR_schools_parents_1415.csv")

hsr_data<-rbind_all(hsr_list)



#kap has fewer rows from the middle schools for some reason.  So get regional adn
#national from KAMS

hsr_national <- hsr_data %>%
  filter(grepl("National", measure_names),
         grepl("Primary", school)) %>%
  mutate(school="KIPP Network",
         measure_names = stringr::str_replace(measure_names, "National ", ""))

hsr_region <- hsr_data %>%
  filter(grepl("Region", measure_names),
         grepl("Primary", school)) %>%
  mutate(school="KIPP Chicago",
         measure_names = stringr::str_replace(measure_names, "Regional ", ""),
         measure_names = stringr::str_replace(measure_names, "Region ", ""))

hsr_schools <- hsr_data %>%
  filter(grepl("School", measure_names)) %>%
  mutate(measure_names = stringr::str_replace(measure_names, "School ", ""))


hsr<-rbind_list(hsr_national, hsr_region, hsr_schools) %>%
  mutate(measure_names=ifelse(grepl("Likert 5", measure_names), "Strongly Agree", measure_names),
         measure_names=ifelse(grepl("Likert 4", measure_names), "Agree", measure_names),
         measure_names=ifelse(grepl("Likert 3", measure_names), "Neutral", measure_names),
         measure_names=ifelse(grepl("Likert 2", measure_names), "Disagree", measure_names),
         measure_names=ifelse(grepl("Likert 1", measure_names), "Strongly Disagree", measure_names),
         measure_names=ifelse(grepl("ositive", measure_names), "Pct Positive", measure_names),
         measure_names=ifelse(grepl("Responded", measure_names), "Num Responded", measure_names))



# HSR High Low Quesitons ####
hsr.hilo <- hsr %>%
  filter(school=="KIPP Chicago",
         measure_names=="Average"
  ) %>%
  arrange(role, measure_values) %>%
  group_by(role) %>%
  mutate(Avg_Rank=row_number(measure_values)) %>%
  filter(Avg_Rank==max(Avg_Rank) | Avg_Rank==min(Avg_Rank)) %>%
  mutate(Rank=ifelse(Avg_Rank!=1, "High", "Low"),
         Rank2=factor(as.character(Rank),levels=c("Low","High"), ordered=T)
  )


hsr.hilo.kap <- hsr %>%
  filter(grepl("Ascend Primary", school),
         measure_names=="Average"
  ) %>%
  arrange(role, measure_values) %>%
  group_by(role) %>%
  mutate(Avg_Rank=row_number(measure_values)) %>%
  filter(Avg_Rank==max(Avg_Rank) | Avg_Rank==min(Avg_Rank)) %>%
  mutate(Rank=ifelse(Avg_Rank!=1, "High", "Low"),
         Rank2=factor(as.character(Rank),levels=c("Low","High"), ordered=T)
  )

hsr.hilo.kams <- hsr %>%
  filter(grepl("Ascend Middle", school),
         measure_names=="Average"
  ) %>%
  arrange(role, measure_values) %>%
  group_by(role) %>%
  mutate(Avg_Rank=row_number(measure_values)) %>%
  filter(Avg_Rank==max(Avg_Rank) | Avg_Rank==min(Avg_Rank)) %>%
  mutate(Rank=ifelse(Avg_Rank!=1, "High", "Low"),
         Rank2=factor(as.character(Rank),levels=c("Low","High"), ordered=T)
  )






hsr.hilo.kccp <- hsr %>%
  filter(grepl("KIPP Create", school),
         measure_names=="Average"
  ) %>%
  arrange(role, measure_values) %>%
  group_by(role) %>%
  mutate(Avg_Rank=row_number(measure_values)) %>%
  filter(Avg_Rank==max(Avg_Rank) | Avg_Rank==min(Avg_Rank)) %>%
  mutate(Rank=ifelse(Avg_Rank!=1, "High", "Low"),
         Rank2=factor(as.character(Rank),levels=c("Low","High"), ordered=T)
  )

hsr.hilo.kbcp <- hsr %>%
  filter(grepl("KIPP Bloom", school),
         measure_names=="Average"
  ) %>%
  arrange(role, measure_values) %>%
  group_by(role) %>%
  mutate(Avg_Rank=row_number(measure_values)) %>%
  filter(Avg_Rank==max(Avg_Rank) | Avg_Rank==min(Avg_Rank)) %>%
  mutate(Rank=ifelse(Avg_Rank!=1, "High", "Low"),
         Rank2=factor(as.character(Rank),levels=c("Low","High"), ordered=T)
  )






# Plot questions ####
p.sample_questions_colors<-ggplot(hsr.hilo, aes(x=0, y=factor(Rank2))) +
  geom_text(aes(label=survey_question, color=Rank), hjust=0) +
  facet_grid(role ~ ., drop = T, scales="free_y") +
  scale_color_manual(values = c("#439539",
                                "#C49A6C")) +
  theme_bw() +
  theme(legend.position="bottom",
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.grid=element_blank(),
        strip.text.y=element_text(size=9),
        legend.margin=unit(.005, "mm"),
        axis.ticks.margin=unit(.005, "mm")) +
  xlim(0,10)


# HSR style summaries ###
# Regione ####
g.stu<-arrangeGrob(
  textGrob("Students who would recommend\nKIPP to their friends and family."),
  tableGrob(data.frame("KIPPChicago"=3.8, "KIPPNational"=3.6),
            cols=c("KIPP Chicago", "KIPP Network"),
            rows = NULL),
  textGrob("64% of students agree that\nthey would recommend KIPP."),
  ncol=1,
  nrow=3
)

g.stu.2<-gList(rectGrob(x=.5, y=.5, width=unit(1, "native")), g.stu)

g.famlies<-arrangeGrob(
  textGrob("Families who are satisfied or very\nsatisfied with their school."),
  tableGrob(data.frame(Region=4.4, Network=4.4),
            cols=c("KIPP Chicago", "KIPP Network"),
            rows = NULL),
  textGrob("89% of families are satisfied\nwith their child's school."),
  ncol=1,
  nrow=3
)
g.families.2<-gList(rectGrob(x=.5, y=.5, width=unit(1, "native")), g.famlies)


g.teachers<-arrangeGrob(
  textGrob("Teachers who would recommend\nKIPP as a great place to work."),
  tableGrob(data.frame(Region=4.1, Network=3.7),
            cols=c("KIPP Chicago", "KIPP Network"),
            rows = NULL),
  textGrob("83% of teachers would\n recommend KIPP as a great\nplace to work."),
  ncol=1,
  nrow=3
)

g.teachers.2<-gList(rectGrob(x=.5, y=.5, width=unit(1, "native")), g.teachers)


g.tables<-arrangeGrob(gTree(children = g.stu.2),
                      gTree(children = g.families.2),
                      gTree(children = g.teachers.2),
                      nrow=1,
                      ncol=3,
                      widths=unit(c(2.75, 2.75, 2.75), "in")
)




# KAP ####
g.stu<-arrangeGrob(
  textGrob("Students who would recommend\nKIPP to their friends and family."),
  tableGrob(data.frame("KIPPChicago"=4.2, "KIPPNational"=3.6),
            cols=c("KAP", "KIPP Network"),
            rows = NULL),
  textGrob("78% of students agree that\nthey would recommend KIPP."),
  ncol=1,
  nrow=3
)

g.stu.2<-gList(rectGrob(x=.5, y=.5, width=unit(1, "native")), g.stu)

g.famlies<-arrangeGrob(
  textGrob("Families who are satisfied or very\nsatisfied with their school."),
  tableGrob(data.frame(Region=4.5, Network=4.4),
            cols=c("KAP", "KIPP Network"),
            rows = NULL),
  textGrob("94% of families are satisfied\nwith their child's school."),
  ncol=1,
  nrow=3
)
g.families.2<-gList(rectGrob(x=.5, y=.5, width=unit(1, "native")), g.famlies)


g.teachers<-arrangeGrob(
  textGrob("Teachers who would recommend\nKIPP as a great place to work."),
  tableGrob(data.frame(Region=4.3, Network=3.7),
            cols=c("KAP", "KIPP Network"),
            rows = NULL),
  textGrob("95% of teachers would\n recommend KIPP as a great\nplace to work."),
  ncol=1,
  nrow=3
)

g.teachers.2<-gList(rectGrob(x=.5, y=.5, width=unit(1, "native")), g.teachers)


g.tables.kap<-arrangeGrob(gTree(children = g.stu.2),
                      gTree(children = g.families.2),
                      gTree(children = g.teachers.2),
                      nrow=1,
                      ncol=3,
                      widths=unit(c(2.75, 2.75, 2.75), "in")
)


# KAMS ####
g.stu<-arrangeGrob(
  textGrob("Students who would recommend\nKIPP to their friends and family."),
  tableGrob(data.frame("KIPPChicago"=3.8, "KIPPNational"=3.6),
            cols=c("KAMS", "KIPP Network"),
            rows = NULL),
  textGrob("60% of students agree that\nthey would recommend KIPP."),
  ncol=1,
  nrow=3
)

g.stu.2<-gList(rectGrob(x=.5, y=.5, width=unit(1, "native")), g.stu)

g.famlies<-arrangeGrob(
  textGrob("Families who are satisfied or very\nsatisfied with their school."),
  tableGrob(data.frame(Region=4.3, Network=4.4),
            cols=c("KAMS", "KIPP Network"),
            rows = NULL),
  textGrob("89% of families are satisfied\nwith their child's school."),
  ncol=1,
  nrow=3
)
g.families.2<-gList(rectGrob(x=.5, y=.5, width=unit(1, "native")), g.famlies)


g.teachers<-arrangeGrob(
  textGrob("Teachers who would recommend\nKIPP as a great place to work."),
  tableGrob(data.frame(Region=3.6, Network=3.7),
            cols=c("KAMS", "KIPP Network"),
            rows = NULL),
  textGrob("63% of teachers would\n recommend KIPP as a great\nplace to work."),
  ncol=1,
  nrow=3
)

g.teachers.2<-gList(rectGrob(x=.5, y=.5, width=unit(1, "native")), g.teachers)


g.tables.kams<-arrangeGrob(gTree(children = g.stu.2),
                          gTree(children = g.families.2),
                          gTree(children = g.teachers.2),
                          nrow=1,
                          ncol=3,
                          widths=unit(c(2.75, 2.75, 2.75), "in")
)

# KCCP ####
g.stu<-arrangeGrob(
  textGrob("Students who would recommend\nKIPP to their friends and family."),
  tableGrob(data.frame("KIPPChicago"=3.6, "KIPPNational"=3.6),
            cols=c("KCCP", "KIPP Network"),
            rows = NULL),
  textGrob("58% of students agree that\nthey would recommend KIPP."),
  ncol=1,
  nrow=3
)

g.stu.2<-gList(rectGrob(x=.5, y=.5, width=unit(1, "native")), g.stu)

g.famlies<-arrangeGrob(
  textGrob("Families who are satisfied or very\nsatisfied with their school."),
  tableGrob(data.frame(Region=4.4, Network=4.4),
            cols=c("KCCP", "KIPP Network"),
            rows = NULL),
  textGrob("90% of families are satisfied\nwith their child's school."),
  ncol=1,
  nrow=3
)
g.families.2<-gList(rectGrob(x=.5, y=.5, width=unit(1, "native")), g.famlies)


g.teachers<-arrangeGrob(
  textGrob("Teachers who would recommend\nKIPP as a great place to work."),
  tableGrob(data.frame(Region=4.2, Network=3.7),
            cols=c("KCCP", "KIPP Network"),
            rows = NULL),
  textGrob("71% of teachers would\n recommend KIPP as a great\nplace to work."),
  ncol=1,
  nrow=3
)

g.teachers.2<-gList(rectGrob(x=.5, y=.5, width=unit(1, "native")), g.teachers)


g.tables.kccp<-arrangeGrob(gTree(children = g.stu.2),
                          gTree(children = g.families.2),
                          gTree(children = g.teachers.2),
                          nrow=1,
                          ncol=3,
                          widths=unit(c(2.75, 2.75, 2.75), "in")
)

# KBCP ####
g.stu<-arrangeGrob(
  textGrob("Students who would recommend\nKIPP to their friends and family."),
  tableGrob(data.frame("KIPPChicago"=3.5, "KIPPNational"=3.6),
            cols=c("KBCP", "KIPP Network"),
            rows = NULL),
  textGrob("52% of students agree that\nthey would recommend KIPP."),
  ncol=1,
  nrow=3
)

g.stu.2<-gList(rectGrob(x=.5, y=.5, width=unit(1, "native")), g.stu)

g.famlies<-arrangeGrob(
  textGrob("Families who are satisfied or very\nsatisfied with their school."),
  tableGrob(data.frame(Region=4.3, Network=4.4),
            cols=c("KBCP", "KIPP Network"),
            rows = NULL),
  textGrob("85% of families are satisfied\nwith their child's school."),
  ncol=1,
  nrow=3
)
g.families.2<-gList(rectGrob(x=.5, y=.5, width=unit(1, "native")), g.famlies)


g.teachers<-arrangeGrob(
  textGrob("Teachers who would recommend\nKIPP as a great place to work."),
  tableGrob(data.frame(Region=4.1, Network=3.7),
            cols=c("KBCP", "KIPP Network"),
            rows = NULL),
  textGrob("80% of teachers would\n recommend KIPP as a great\nplace to work."),
  ncol=1,
  nrow=3
)

g.teachers.2<-gList(rectGrob(x=.5, y=.5, width=unit(1, "native")), g.teachers)


g.tables.kbcp<-arrangeGrob(gTree(children = g.stu.2),
                          gTree(children = g.families.2),
                          gTree(children = g.teachers.2),
                          nrow=1,
                          ncol=3,
                          widths=unit(c(2.75, 2.75, 2.75), "in")
)

