#require(ProjectTemplate)

#if(!"Attendance" %in% project.info$data) load.project()

##### Daily Attendence ####
# By School
DailyEnrollAttend<-Attendance[,list(Enrolled=sum(Enrolled), Absent=sum(Absent)), by=list(School, Date)]

# By Grade
DailyEnrollAttendByGrade<-Attendance[,list(Enrolled=sum(Enrolled), Absent=sum(Absent)), by=list(School, Grade, Date)]

DEA.list<-list(DailyEnrollAttend=DailyEnrollAttend, 
               DailyEnrollAttendByGrade=DailyEnrollAttendByGrade)

DEA.list<-lapply(DEA.list, function(x) x<- prep_Att_Summary_Tables(x))


DailyEnrollAttend<-DEA.list$DailyEnrollAttend
DailyEnrollAttendByGrade<-DEA.list$DailyEnrollAttendByGrade



#### Calculate Attendance rate by week by school ####


AttRateByWeekBySchool<-DailyEnrollAttend[,list(AttRate=sum(Present)/sum(Enrolled)*100), 
                                         by=list(School, WeekOfShortDateLabel)][order(School)]
  
AttRateYTDBySchool<-DailyEnrollAttend[,list(AttRate=sum(Present)/sum(Enrolled)*100), 
                  by=list(School)][order(School)]




AttRateYTDBySchool[,WeekOfShortLabel:="YTD Each School"]

AttRateByWeekBySchool.table<-cast(AttRateByWeekBySchool, WeekOfShortDateLabel ~ School, value=.(AttRate))

AttRateYTDBySchool<-reshape(AttRateYTDBySchool, idvar="WeekOfShortLabel",timevar="School", direction="wide")
setnames(AttRateYTDBySchool, c("WeekOfShortDateLabel", "KAP", "KAMS", "KCCP", "KBCP"))




AttRateYTDRegion<-data.frame(WeekOfShortDateLabel="YTD Region", 
                             KAP=DailyEnrollAttend[,sum(Present)/sum(Enrolled)*100], 
                             KAMS=NA,
                             KCCP=NA, 
                             KBCP=NA)

AttRateYTDKAPKAMS<-data.frame(WeekOfShortDateLabel="YTD KAP & KAMS", 
                              KAP=DailyEnrollAttend[School %in% c("KAMS", "KAP"),sum(Present)/sum(Enrolled)*100], 
                              KAMS=NA,
                              KCCP=NA, 
                              KBCP=NA)


AttRateByWeekBySchool.table<-rbind(AttRateByWeekBySchool.table,AttRateYTDBySchool,AttRateYTDKAPKAMS, AttRateYTDRegion)
AttRateByWeekBySchool.table[,c(2:5)] <- round(AttRateByWeekBySchool.table[,c(2:5)],1)  




setnames(AttRateByWeekBySchool.table, 1, "Week of") #a better column title


#### Attendance by Student by School ####
AttByStudentBySchool<-
  copy(Attendance[CurrentStatus==1,list(Absences=sum(Absent), 
                                       ADA=round((1-(sum(Absent)/sum(Enrolled)))*100,1)), 
                 by=list(StudentID,Student, 
                         School, 
                         Grade)
                 ][,list(StudentID,Student,
                         ADA, 
                         Absences,
                         Pctl=rank(ADA)/.N),
                   by=list(School, 
                           Grade)
                   ][Pctl<=.10][order(School, Grade, ADA)]
  )

ADA_28.dt<-
  Attendance[Date>=ymd(as.character(today() - days(28)))][,list(ADA_28=round(100*(1-(sum(Absent)/.N)),1)), 
                                                          by=list(StudentID)]

setkey(ADA_28.dt, StudentID)
setkey(AttByStudentBySchool, StudentID)

AttByStudentBySchool<-copy(ADA_28.dt[AttByStudentBySchool][order(School, Grade, ADA)][,list(School, Grade, Student, ADA, ADA_28, Absences)])


setnames(AttByStudentBySchool, c("School", "Grade", "Student","ADA", "ADA (prior month)"  ,"Absences"))

