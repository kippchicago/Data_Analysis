school_abbrev <- function(.data){
  
  look_up <- function(x){
    z <- switch(as.character(x), 
                "78102"  = "KAP",
                "7810"   = "KAMS",
                "400146" = "KCCP",
                "400163" = "KBCP"
    )
    z
  }
  out <- mapply(look_up, .data)
  as.character(out)
}

prep_Att_Summary_Tables <- function(.data) {
  
  # Takes Daily Attendence summary tables and calcuatlaes Number Present, Percent Absent, Percent Presnet (=ADA), 95% 
  # attendence threshold (based on current day's enrollment) and a bounty of Week of dates with numbers and labels. 
  # Arguments:
  #             .data:  a data.table object with the fields Enrolled, Absent, and Date
  # Returns:
  #             x:      a data.table object identical to .data with the addition of the calculated columns 
  
  stopifnot(#is.data.table(.data), 
    c("Enrolled", "Absent", "Date") %in% names(.data)
  )
  
  x<-.data %>%
    mutate(Present=Enrolled-Absent, #quick daily facts
           PctAbsent=Absent/Enrolled,
           PctPresent=1-PctAbsent,
           PctPresentGTE95=PctPresent>=.95,
           WeekInYear=week(Date)
    ) %>% 
    group_by(School) %>% #Week of calculations and labels
    mutate(WeekInSchoolYear=(floor_date(Date, unit="week") 
                             - min(floor_date(Date, unit="week")))/dweeks(1)+1,
           WeekOfDate=floor_date(Date, unit="week") + days(1),
           WeekOfShortDateLabel=paste(
             lubridate::month(WeekOfDate,label=TRUE, abbr=TRUE), 
             lubridate::day(WeekOfDate), 
             sep=" ")
    ) %>% ungroup %>%
    arrange(WeekInSchoolYear) %>% #resort
    mutate(WeekOfShortDateLabel=factor(WeekInSchoolYear, labels=unique(WeekOfShortDateLabel))) #Short Week  Label
  
  x
  
  
}