# This file holds custom functions used in the Board Report Attrition Project.
# Function in here should be close to one off.  Otherwise we should start 
# writing a package that provides functionality to any data set  

# Christoher Haid for KIPP Chicago
# March 2013

roster.kams[grade==8 & month==6 & year==2012][roster.kams[grade==5 & month==10 &year ==2008]][,list(n.5=.N, n.8=sum(!is.na(grade)))][,class:=2016]

#Need to get a data.table with full roster in October of 5th grade and then remaining roster for each subsequent grade. 
roster.match <- function (roster, class.year) {
  # roster.match takes a data.table with rosters for a given month-year and
  # returns a data.table with the full roster for the first date and the 
  # students who persist from the orginal roster on each subsequent date; this 
  # funtion is used by KIPP Chicago to calcuate cohort persistnace, i.e., what
  # percentage of students that start in fall 5th grade (on or round October 1)
  # remain each year.
  # Args:
  #   roster: a data.frame or data.table (latter preferred) with PowerSchool ID
  #           (ps.id), name, grade, date, month, year.  ps.id, drade, month, and
  #           year are required fields. 
  # Returns:
  #   A data.table with ps.id for each student in original cohort (by earliest
  #   date) and name, grade, date, month, year data if the student remained on 
  #   subsequent rosters.  If the student dropped off the roster then those 
  #   fields are null. 
  require(data.table)
  
  # Test that date is data.table, if not recatst as data.table 
  if (!is.data.table(roster)) {
    roster<-data.table.roster
  }
  
  end.year <- class.year - 4
  start.year <- end.year - 4
  sy.start.month <- 10
  sy.end.month <- 6
  g <- 5
  counter <- 1
  
  # set key to ps.id
  setkey(roster, ps.id)  # ps.id is PowerSchool ID (ID in STUDENTS TABLE)
  # Initialize outpute data frame with first roster
  r.out<-roster[grade == g & 
                month == 10 &
                 year == start.year
                ]
  
  start.year<-start.year+1
  
  #now each beginning of year roster 
  for (y in c(start.year:end.year)) {
    if (y == end.year){
      #g <- g + 1
      r.out<-roster[grade == g &
                    month == 6 &
                    year == y][r.out]
    }
    else {
      g<-g + 1
      r.out<-roster[grade == g &
                    month == 10 &
                    year == y][r.out]
    }  
  }
  return(r.out)
}
  
  roster.2016<-roster.kams[
    grade == 8 & 
      month == 6 & 
      year  == 2012][roster.kams[
                      grade == 8 & 
                      month == 10 & 
                      year  == 2011][roster.kams[
                    grade == 7 & 
                    month == 10 & 
                    year  == 2010][roster.kams[
                                    grade == 6 & 
                                    month == 10 & 
                                    year  == 2009][roster.kams[
                                                    grade == 5 & 
                                                    month == 10 &
                                                    year  == 2008]
                                                   ]
                                   ]
                                ]
                     ]


                   
