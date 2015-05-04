# This script creates summary graphics for Illuminate assessement
# data (weekly and unit).  

# Pre-reqs ####
setwd("~/Dropbox (KIPP Chicago Schools)/Data Analysis/Illuminate/Data_Reporting_Prototypes")
require(ProjectTemplate)
load.project()

# Instantiate a logger object
logger <- log4r::create.logger()

# Set the logger's file output: currently only allows flat files.
logfile(logger) <- file.path('logs/base.log')

# Set the current level of the logger.
level(logger) <- "INFO"

# Database ###
info(logger, "Reading Silo Config")
silo<-as.data.frame(read.dcf('config//silo_dw.dcf'))

info(logger, "Connecting to Silo")
drvr <- JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver',
             '~/Dropbox (KIPP Chicago Schools)/JDBC Drivers/sqljdbc_4.0/enu//sqljdbc4.jar')

conn <- dbConnect(drvr,
                  databaseName=as.character(silo$dbname),
                  url=as.character(silo$url),
                  user=as.character(silo$user),
                  password=as.character(silo$password)
)


info(logger, "Getting Illumiante DnA Tables")

qry<-"
SELECT stus.*, 
ill.*
FROM Illuminate_mirror..assessment_results_by_question_group_detail ill
LEFT JOIN (SELECT schoolid,  
grade_level,
student_number,
lastfirst,
enroll_status=0
FROM PS_mirror..students
--WHERE enroll_status=0
) stus
ON ill.local_student_id=stus.student_number
WHERE schoolid<>999999 
AND enroll_status=0
"

assessments_by_objective<-dbGetQuery(conn, qry)
glimpse(assessments_by_objective)


assess_data<-str_split(assessments_by_objective$local_assessment_id, pattern = "\\.")
assess_data <-do.call(rbind.data.frame, assess_data)
colnames(assess_data)<-c("test_grade", 
                         "test_subject", 
                         "test_week_number", 
                         "test_type", 
                         "test_unit_number", 
                         "test_date", 
                         "test_school")

#assess_data <- assess_data %>%
#  mutate(test_school = ifelse(!is.na(as.integer(test_school)), 
#                              "All", test_school
#  )
#  )

assessments_by_objective_1<-cbind(assessments_by_objective, assess_data)

# Let's subset to 5th grade science ####
info(logger, "Subsetting to Science, KAP, 5th grade, unit asssessments only")
assessments <- assessments_by_objective_1 %>%
  mutate(mastered=as.integer(mastered)) %>%
  filter(test_subject=="SCI", 
         #grepl("14-15", title),
         #grepl("Unit", title),
         #grepl("Assessment", title),
         #schoolid==78102,
         academic_year==2015,
         test_grade==5
  )


glimpse(assessments)

# get test data form local



#assessments_wide <- assessments %>%
#  filter(assessment_id==7223) %>%
#  select(lastfirst, grade_level, reporting_group, percent_correct) %>%
#  spread(reporting_group, percent_correct)

#assessments_wide %>% head

# This function divides long strings into two by separating parts by a 
# carriage return.
str_add_carriage_return <- function(string, n=2){
  inner<-function(str, n=2){
    x<-str_locate_all(str, " ")
    n_spaces <- nrow(x[[1]])
    n_spaces
    split_locations <- round(n_spaces/n)
    split_here<-x[[1]][split_locations,"start"]
    part1<-str_sub(str, 1, split_here)
    part2<-str_sub(str, split_here+1, str_length(str))
    out<-paste(part1, part2, sep="\n")
    out
  }
  
  out2<-mapply(inner, 
               string, 
               MoreArgs = list(n=2),
               SIMPLIFY = TRUE,
               USE.NAMES = FALSE
               ) 
  
  as.vector(out2, mode="character")  
}




assessments_2<- assessments%>%
  mutate(Mastered=mastered==1, 
         Objective=str_add_carriage_return(reporting_group),
         label=paste0(round(percent_correct,1),
                      "% (",
                      points,
                      "/",
                      points_possible,
                      ")"
                      )
  )

totals_by_student<-assessments_2 %>% 
  group_by(schoolid, student_number, lastfirst) %>% 
  dplyr::summarize(points_possible=n(), 
            points=sum(mastered), 
            percent_correct=points/points_possible*100, 
            title="Objectives\nMastered\nby Student", 
            #schoolid=78102, 
            Objective="Mastered") %>%
  mutate(Mastered=percent_correct>=80,
         label=paste0(round(percent_correct,1),
                      "% (",
                      points,
                      "/",
                      points_possible,
                      ")"
                      )
         )

assessments_3<-rbind_all(list(assessments_2, totals_by_student))

totals_by_objective <- assessments_3 %>%
  group_by(Objective, title) %>%
  dplyr::summarize(points_possible=n(), 
            points=sum(mastered), 
            percent_correct=points/points_possible*100
            ) %>%
  mutate(lastfirst="Students Mastery \nof Objective",
         schoolid=0, 
         Mastered=percent_correct>=80,
         label=paste0(round(percent_correct,1),
                      "% (",
                      points,
                      "/",
                      points_possible,
                      ")"
         )
  )

assessments_4<-rbind_all(list(assessments_3, totals_by_objective)) %>%
  mutate(School=ifelse(schoolid==78102, "KAP", "Objective Mastery"))

info(logger, "Ordering Assessments by Date")
  assessments_dates<-assessments_4 %>%
  mutate(administered_at=ifelse(title=="Objectives\nMastered\nby Student",
                                as.character(today()), 
                                administered_at),
         date=ymd(administered_at)) %>%
  filter(!is.na(date)) %>%
  group_by(local_assessment_id) %>%
  dplyr::summarize(Date=min(date)) %>%
  arrange(Date)

assessments_5 <- assessments_4 %>%
  mutate(Test_ID=factor(as.character(local_assessment_id), 
                        levels = as.character(assessments_dates$local_assessment_id)
                        )
         )


info(logger, "Plotting mastery")
p<-ggplot(assessments_5 %>% filter(test_unit_number==4, 
                                   test_school %in% c("KAP", 
                                                      "ALL"),
                                   School =="KAP"
                                   ), 
       aes(x=Objective, 
           y=lastfirst)) + 
  geom_tile(aes(fill=Mastered),
            color="white") +
  geom_text(aes(label=label), 
            size=1, 
            color="lightgray") +
  facet_grid(School~Test_ID, scales = "free", space =  "free") +
  scale_fill_manual(values = c("#E27425", "#439539")) +
  theme_bw() + 
  theme(axis.text.y = element_text(size=4.5),
        axis.text.x = element_text(size=4.5, angle=45, hjust=1), 
        strip.text.x = element_text(size=5),
        strip.text.y=element_text(size=5, angle=0))

p

pdf(file="graphs/mastery_by_objective_2.pdf", height = 8.25, width = 10.75)
  p + ggtitle("Science, KAP, 5th Grade, Unit and Weekly Tests\n PROOF OF CONCEPT")
dev.off()

# Roll-up by School ###
assessments_school_level <- assessments_by_objective_1 %>%
  mutate(mastered=as.integer(mastered)) %>%
  filter(test_unit_number==4,
         test_subject=="SCI",
         academic_year==2015,
         grade_level==5
  )




debug(logger, "Figuring out which tests are unit test and which aren't.")
# need to use dates of unit identified tests to "corale" 
# weekly assessments.
science_assessmets<-assessments_by_objective_1 %>% 
 filter(test_subject=="SCI", 
         test_grade=="5",
         academic_year==2015, 
         test_unit_number=="4") %>% 
  arrange(administered_at) %>% 
   mutate(Title=ifelse(test_type=="U", paste("Unit", test_unit_number, "Test: Week", test_week_number),
                       paste("Weekly Test: Week", test_week_number)
                       )
          )  

schools <- data.frame(schoolid=c(78102, 7810, 400146, 400163),
                      school=c("KAP", "KAMS", "KCCP", "KBCP")
                      )

a_sl<-science_assessmets %>% 
  mutate(Mastered=is_mastery==1,
         administered_at=ifelse(title=="Objectives\nMastered\nby Student",
                                as.character(today()), 
                                administered_at),
         date=ymd(administered_at)
         ) %>%
  group_by(Title, schoolid,  reporting_group) %>% 
  dplyr::summarize(N=n(), 
            date=min(date),       
            N_Mastered = sum(Mastered), 
            Pct_Mastered=N_Mastered/N*100) %>%
  mutate(Mastery_Cat=cut(Pct_Mastered, c(0,50,75,100)))

assessments_ordered <- a_sl %>% 
  ungroup %>%
  select(Title, date) %>% 
  arrange(date) %>%
  select(Title) %>% unique

a_sl_ordered <- a_sl %>% ungroup %>%
  mutate(Title =  factor(Title, levels = assessments_ordered$Title))
  


# Histogram ###
p2<-ggplot(a_sl_ordered, aes(x=reporting_group, y=Pct_Mastered)) + 
  geom_histogram(aes(fill=Mastery_Cat), stat="identity") +
  geom_text(aes(label=round(Pct_Mastered)),
            hjust=1,
            color="white",
            vjust=.5) +
  facet_grid(schoolid ~ Title ) + 
  #facet_grid(Title ~ test) + 
  coord_flip() +
  theme_bw() + 
  ylab("% of students mastering objective") + 
  xlab("Objective")

p2

cairo_pdf(filename = "graphs/weekly_and_unit_test.pdf", width=10.75, height=8.25)
  p2 + 
  theme(axis.text.y = element_text(size=6),
        #axis.text.x = element_text(size=4.5, angle=45, hjust=1), 
        strip.text.x = element_text(size=5),
        strip.text.y=element_text(size=5, angle=0),
        legend.position="bottom")
dev.off()

# ggvis version of above

require(ggvis)
  
slider<-input_radiobuttons(c("KAP", "KCCP", "KBCP"), map = function(x) switch(x, KAP=78102, KCCP=400146, KBCP=400163))
  ggvis(a_sl_ordered %>%
          filter(schoolid==slider),
    x=~reporting_group, y=~Pct_Mastered) %>%
  group_by(reporting_group) %>%
  layer_bars(fill=~Mastery_Cat, stack = FALSE)


ggplot(a_sl_ordered, aes(x=reporting_group, y=Pct_Mastered)) + 
  geom_histogram(aes(fill=Mastery_Cat), stat="identity") +
  geom_text(aes(label=round(Pct_Mastered)),
            hjust=1,
            color="white",
            vjust=.5) +
  facet_grid(schoolid ~ Title ) + 
  #facet_grid(Title ~ test) + 
  coord_flip() +
  theme_bw() + 
  ylab("% of students mastering objective") + 
  xlab("Objective")



