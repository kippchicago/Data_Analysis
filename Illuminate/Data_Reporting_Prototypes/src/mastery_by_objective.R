# This script creates summary graphics for Illuminate assessement
# data (weekly and unit).  

# Pre-reqs ####
setwd("~/Dropbox (KIPP Chicago Schools)/Data Analysis/Illuminate/Data_Reporting_Prototypes")
require(ProjectTemplate)
load.project()

# Instantiate a logger object
logger <- create.logger()

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


# Let's subset to 5th grade science ####
info(logger, "Subsetting to Science, KAP, 5th grade, unit asssessments only")
assessments <- assessments_by_objective %>%
  mutate(mastered=as.integer(mastered)) %>%
  filter(subject_area=="Science", 
         #grepl("14-15", title),
         #grepl("Unit", title),
         #grepl("Assessment", title),
         schoolid==78102,
         academic_year==2015,
         grade_level==5
  )


#glimpse(assessments)


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
               SIMPLIFY = TRUE)
  out2
  
}




assessments_2<- assessments %>%
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
  group_by(student_number, lastfirst) %>% 
  summarize(points_possible=n(), 
            points=sum(mastered), 
            percent_correct=points/points_possible*100, 
            title="Objectives\nMastered\nby Student", 
            schoolid=78102, 
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
  summarize(points_possible=n(), 
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
  group_by(title) %>%
  summarize(Date=min(date)) %>%
  arrange(Date)

assessments_5 <- assessments_4 %>%
  mutate(Title=factor(as.character(title), levels = as.character(assessments_dates$title)))


info(logger, "Plotting mastery")
p<-ggplot(assessments_5, 
       aes(x=Objective, 
           y=lastfirst)) + 
  geom_tile(aes(fill=Mastered),
            color="white") +
  geom_text(aes(label=label), 
            size=1, 
            color="lightgray") +
  facet_grid(School~Title, scales = "free", space =  "free") +
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


