# Helper scripts for Weekly Attendence Data

prep_Att_Summary_Tables <- function(.data) {
  
  # Takes Daily Attendence summary tables and calcuatlaes Number Present, Percent Absent, Percent Presnet (=ADA), 95% 
  # attendence threshold (based on current day's enrollment) and a bounty of Week of dates with numbers and labels. 
  # Arguments:
  #             .data:  a data.table object with the fields Enrolled, Absent, and Date
  # Returns:
  #             x:      a data.table object identical to .data with the addition of the calculated columns 
  
  stopifnot(is.data.table(.data), 
            c("Enrolled", "Absent", "Date") %in% names(.data)
            )
  
  x<-copy(.data)
  
  #Some quick daily stats
  x[,Present:=Enrolled-Absent]
  
  x[,PctAbsent:=Absent/Enrolled]
  
  x[,PctPresent:=1-PctAbsent]
  
  x[,PctPresentGTE95:=PctPresent>=.95]
  
  #Week of calculations and labels
  
  x[,WeekInYear:=week(Date)]
  
  
  x[,WeekInSchoolYear:=(floor_date(Date, unit="week") 
                        - min(floor_date(Date, unit="week")))/dweeks(1)+1]
  
  
  x[,WeekOfDate:=floor_date(Date, unit="week") + days(1) ]
  
  
  #Short Week  Label
  x[, WeekOfShortDateLabel:=paste(
    lubridate::month(WeekOfDate,label=TRUE, abbr=TRUE), 
    day(WeekOfDate), 
    sep=" ")]
  
  x[, WeekOfShortDateLabel:=factor(WeekInSchoolYear, labels=unique(WeekOfShortDateLabel))]
  
  
}
