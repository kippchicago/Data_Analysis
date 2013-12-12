#### Suspensions Tables ####


Suspensions<-prep_Att_Summary_Tables(Attendance[AttCode=='S'])

#### Daily Suspensions By Grade ####
DailySuspensionByGradeByWeek<-copy(Suspensions[,list(Suspended=sum(Absent)), 
                                               by=list(School, Grade, WeekOfShortDateLabel)])


# Weekly suspension by school 
WeeklySuspensionsBySchool.table<-cast(DailySuspensionByGradeByWeek, WeekOfShortDateLabel~School, sum, margins=TRUE)

# Change Week of row (all) to Total
levels(WeeklySuspensionsBySchool.table$WeekOfShortDateLabel)[levels(WeeklySuspensionsBySchool.table$WeekOfShortDateLabel)=="(all)"]<-"Total"

setnames(WeeklySuspensionsBySchool.table, c("WeekOfShortDateLabel","(all)"), 
         c("Week of", "Total"))
levels(WeeklySuspensionsBySchool.table[,1])[levels(WeeklySuspensionsBySchool.table[,1])=="(all)"]<-"Total"

WeeklySuspensionsBySchool.xtable<-xtable(WeeklySuspensionsBySchool.table, digits=0)


#YTD Suspsesnions by Grade By School
YTDSuspensionsByGradeBySchool.table<-cast(DailySuspensionByGradeByWeek, Grade~School, sum, margins=TRUE)

setnames(YTDSuspensionsByGradeBySchool.table, "(all)", "Total")

levels(YTDSuspensionsByGradeBySchool.table[,1])[levels(YTDSuspensionsByGradeBySchool.table[,1])=="(all)"]<-"Total"

YTDSuspensionsByGradeBySchool.xtable<-xtable(YTDSuspensionsByGradeBySchool.table, digits=0)



#YTD Suspsesnions by Grade by Week
YTDSuspensionsByWeekByGrade.table<-data.table(cast(DailySuspensionByGradeByWeek, WeekOfShortDateLabel~Grade, sum, margins=TRUE))

YTDSuspensionsByWeekByGrade.table[WeekOfShortDateLabel=="(all)",WeekOfShortDateLabel:="Total"]

setnames(YTDSuspensionsByWeekByGrade.table, c("WeekOfShortDateLabel", "(all)"), c("Week of", "Total"))


YTDSuspensionsByWeekByGrade.xtable<-xtable(YTDSuspensionsByWeekByGrade.table, digits=0)



Sups.leaders<-Suspensions[,list(SuspendedDays=sum(Absent)), by=list(Student, School, Grade)][order(desc(SuspendedDays))]
setnames(Sups.leaders, "SuspendedDays", "Days Suspended")