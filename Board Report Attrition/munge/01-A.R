# Pre-processing script 

# Rename columns to lowercase letters and and shorter names were possible 
# (i.e. GRADE_LEVEL to grade)

setnames(roster.kams, c("student.name", "ps.id", "grade", "date"))

# retype calendar dat with lubridate.  The database returns dates in 
# YYYY-MM-DD hh:mm:ss format where the times are all 00:00:00, so we can 
# dispense with the time in the the date field

roster.kams[, date:=ymd_hms(date)]

# set keys for data.table.  The date and PowerSchool ID should be sufficient for 
# uniqueness
setkey(roster.kams, ps.id)

# add month and year columns for easier joins
roster.kams[, month:=month(date)]
roster.kams[, year:=year(date)]

cache(roster.kams)
