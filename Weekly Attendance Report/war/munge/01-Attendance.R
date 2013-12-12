# Munching/processing script of Attendance table for Weekly Attendence Report
Attendance<-as.data.table(Attendance)

# Assign school initals based on school id
Attendance[SCHOOLID==7810, School:="KAMS"]
Attendance[SCHOOLID==78102, School:="KAP"]
Attendance[SCHOOLID==400146, School:="KCCP"]
Attendance[SCHOOLID==400163, School:="KBCP"]

# change intials from character to factor with new order
Attendance[,School:=factor(School, levels=c("KAP", "KAMS", "KCCP", "KBCP"))]

# change grade level to factor
Attendance[,GRADE_LEVEL := factor(GRADE_LEVEL, 
                                  levels=c(0,1,2,3,5,6,7,8),
                                  labels=c("K",1,2,3,5,6,7,8))]

#Change column names to title case
setnames(Attendance, old=names(Attendance),
         new=c("SchoolID",
               "Grade",
               "Date",
               "StudentID",
               "Student",
               "CurrentStatus",
               "Enrolled",
               "AttCode",
               "AttDescr",
               "PresenceStatusCode",
               "CourseCreditPoints",
               "Absent",
               "School"))

# recast dates as POSIX
Attendance[,Date:=ymd_hms(Date)]

