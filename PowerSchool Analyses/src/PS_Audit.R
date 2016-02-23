# Connect to server using JDBC Connection (note: requires a VPN connection to be open to psshostingvpn.poowerschool.com)
require(RJDBC)

drvr <- JDBC("oracle.jdbc.driver.OracleDriver", "/Users/chaid/Dropbox/JDBC Drivers/ojdbc6.jar","") # define driver

pspw <- as.list(read.dcf("config/ps.dcf", all=TRUE)) #read DCF with configuration settings

pscon <- dbConnect(drvr,pspw$SERVER,pspw$UID,pspw$PWD) # connect to server

# 
sql.statement<-paste("
SELECT 
        s.*, 
        grades.SCHOOLID,
        grades.DATEENROLLED,
        grades.dateleft,
        grades.termid,
        grades.section_number,
        grades.course_number,
        grades.finalgradename,
        grades.grade,
        grades.percent
FROM
        (SELECT      
                 cc.* , 
                 pgf.finalgradename,
                 pgf.grade,
                 pgf.percent
                 FROM        pgfinalgrades pgf
                 LEFT JOIN   cc
                 ON cc.sectionid = pgf.sectionid AND cc.studentID= pgf.studentID
                 WHERE cc.DATEENROLLED >='01-AUG-13'
                 --AND cc.schoolid=7810
                ) grades
LEFT JOIN  (SELECT 
                    ID AS studentid,
                    lastfirst,
                    grade_level,
                    home_room
                    FROM
                    students 
                    WHERE enroll_status=0
                    ) s
ON s.studentID = grades.studentid",
                     sep="")
  
#Execture qurey and return to data frame   
grades<-dbGetQuery(pscon, sql.statement)

# 7th Grade ####
# subset grades to 7th and core courses, select only necessary columns and 
# rename columns for easier analysis
grades_7th<-filter(grades, GRADE_LEVEL==7, 
                   COURSE_NUMBER %in% c("kams7ela",
                                        "kams7sci", 
                                        "kams7wyl", 
                                        "kams7math")) %>%
  select(Name=LASTFIRST, 
         Grade=GRADE_LEVEL, 
         HR=HOME_ROOM,
         Section=SECTION_NUMBER, 
         Course=COURSE_NUMBER,
         Grading_Period=FINALGRADENAME,
         Letter_Grade=GRADE,
         Percent=PERCENT)


#transform grades so that Q1-Q2, Y1 are columns
grades_7th_cast <- dcast(grades_7th, 
                         Name + Grade + HR + Course + Section ~ Grading_Period, 
                         value.var = "Percent")


# need this function to allow roudning up from 5, rather than roudning to the nearest even number
round2 <- function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}


grades_7th_cast<-mutate(grades_7th_cast, 
                        Y1_Correct=round2((Q1+Q2+Q3+Q4)/4, 0), 
                        Diff=Y1_Correct-Y1,
                        Helped=Diff<0,
                        Harmed=Diff>0,
                        Correct=Diff==0)

group_by(grades_7th_cast, Course, HR, Section) %>% 
  summarise(N=n(),
            Total_Helped=sum(Helped),
            Pct_Helped=round(Total_Helped/N*100),
            Total_Harmed=sum(Harmed),
            Pct_Harmed=round(Total_Harmed/N*100),
            Total_Correct=sum(Correct),
            Pct_Correct=round(Total_Correct/N*100)
            ) %>%
  filter(Pct_Correct!=100) %>%
  select(-N, -Section)


# as a function

#Function####
  ps_audit<-function(.data, grade=7, include_letter_grades=FALSE){
    
    if(!missing(grade)) .data<-filter(.data, GRADE_LEVEL==grade)
    grades<- .data %>%
      select(Name=LASTFIRST, 
             Grade=GRADE_LEVEL, 
             HR=HOME_ROOM,
             Section=SECTION_NUMBER, 
             Course=COURSE_NUMBER,
             Grading_Period=FINALGRADENAME,
             Letter_Grade=GRADE,
             Percent=PERCENT)
    
    
    #transform grades so that Q1-Q2, Y1 are columns
    grades_cast <- dcast(grades, 
                             Name + Grade + HR + Course + Section ~ Grading_Period, 
                             value.var = "Percent")
    
    
    # need this function to allow roudning up from 5, rather than roudning to the nearest even number
    round2 <- function(x, n) {
      posneg = sign(x)
      z = abs(x)*10^n
      z = z + 0.5
      z = trunc(z)
      z = z/10^n
      z*posneg
    }
    
    
    grades_cast<-mutate(grades_cast, 
                            Y1_Correct=round2((Q1+Q2+Q3+Q4)/4, 0), 
                            Diff=Y1_Correct-Y1,
                            Helped=Diff<0,
                            Harmed=Diff>0,
                            Correct=Diff==0)
    
    out_summary<-group_by(grades_cast, Course, HR, Section, Grade) %>% 
      summarise(N=n(),
                Total_Helped=sum(Helped),
                Pct_Helped=round(Total_Helped/N*100),
                Total_Harmed=sum(Harmed),
                Pct_Harmed=round(Total_Harmed/N*100),
                Total_Correct=sum(Correct),
                Pct_Correct=round(Total_Correct/N*100)
      ) %>%
      filter(Pct_Correct!=100) %>%
      select(-N, -Section)
    
  if(include_letter_grades){
    letter_grades<-function(x){
      cut(x, 
          breaks=c(0,70,73,77,80,83,87,90,94,98,100), 
          labels=c("F", "C-", "C", "C+", "B-", "B", "B+", "A-", "A", "A+"), 
          right=F)
    }
    
    grades_cast <- mutate(grades_cast, 
                          Y1_Letter=letter_grades(Y1),
                          Y1_Letter_Correct=letter_grades(Y1_Correct)) 
    
  }   
    #return
    out<-list()
    out$summary<-out_summary
    out$students<-arrange(grades_cast, Grade, Name)
    out
  }

# KAMS  ####
#get all KAMS students
grades_kams<-filter(grades, SCHOOLID==7810)

#get all grades
ps_kams <-ps_audit(grades_kams, include_letter_grades = T)
harmed_kams<- ps_kams$students %>% filter(Harmed==T, Y1_Letter!=Y1_Letter_Correct, !grepl("hr", Course))

write.csv(select(harmed_kams, 
                 Name, 
                 Grade, 
                 HR, 
                 Course, 
                 Y1, 
                 Y1_Correct, 
                 Y1_Letter, 
                 Y1_Letter_Correct),
          file="reports/harmed_kams.csv"
)

# KCCP  ####
#get all KAMS students
grades_kccp<-filter(grades, SCHOOLID==400146)

#get all grades
ps_kccp <-ps_audit(grades_kccp, include_letter_grades = T)
harmed_kccp<- ps_kccp$students %>% filter(Harmed==T, Y1_Letter!=Y1_Letter_Correct, !grepl("hr", Course))

write.csv(select(harmed_kccp, 
                 Name, 
                 Grade, 
                 HR, 
                 Course, 
                 Y1, 
                 Y1_Correct, 
                 Y1_Letter, 
                 Y1_Letter_Correct),
          file="reports/harmed_kccp.csv"
)



