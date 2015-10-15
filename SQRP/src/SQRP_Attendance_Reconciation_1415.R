# SQRP reconciliaton

# Bloom
x<-current_membership %>%
  filter(SCHOOLID == 400163, YEARID==24) %>%
  group_by(STUDENT_NUMBER) %>%
  summarize(N=n()) %>%
  arrange(STUDENT_NUMBER)

write.csv(x %>% arrange(STUDENT_NUMBER), file =  "~/Desktop/bloom_enrolled.csv")

# Ascend
x<-current_membership %>%
  filter(grepl("7810", SCHOOLID), YEARID==24, GRADE_LEVEL != 0) %>%
  group_by(STUDENT_NUMBER) %>%
  summarize(N=n()) %>%
  arrange(STUDENT_NUMBER)

write.csv(x %>% arrange(STUDENT_NUMBER), file =  "~/Desktop/ascend_enrolled.csv")


# Create
x<-current_membership %>%
  filter(grepl("400146", SCHOOLID), YEARID==24, GRADE_LEVEL != 0) %>%
  group_by(STUDENT_NUMBER) %>%
  summarize(N=n()) %>%
  arrange(STUDENT_NUMBER)

write.csv(x %>% arrange(STUDENT_NUMBER), file =  "~/Desktop/create_enrolled.csv")
