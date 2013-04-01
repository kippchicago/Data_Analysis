# This R script gets October 1 and June 15 (or nearest weekdays) for each school year form 
# 2005-2006 through 2011-12 at KAMS (all grades).  This singe data table can be used to calcualte who remains after any nubmer o f years from a given 5th grade cohort

# Establish connection to PowerServer database
drvr <- JDBC("oracle.jdbc.driver.OracleDriver", "/Users/chaid/Dropbox/JDBC Drivers/ojdbc5.jar","")
pscon <- dbConnect(drvr,"jdbc:oracle:thin:@10.160.29.47:1521:IL039","psnavigator","laidephy")


# Get rosters  for 2011-2012
cat("Retrieving 10/1 and 6/15 Rosters for 2005 - 2012 from PowerSchool (this can take some time . . . \n ")


# Set up progress bar, since it can be timely
pb <- txtProgressBar(min = 0, max = 16, style = 3)

# last day of school
roster.6.2012<-dbGetQuery(pscon, "
                            Select N.LastFirst, R.StudentID, R.GRADE_LEVEL,CALENDARDATE
                            FROM (
                                  SELECT * 
                                  FROM PS_MEMBERSHIP_DEFAULTS 
                                  WHERE Calendardate='15-JUN-2012' 
                                  AND SCHOOLID = 7810
                                ) R
                          JOIN (
                               SELECT ID, LASTFIRST FROM STUDENTS
                               ) N
                          ON N.ID = R.StudentID
                          "
)

setTxtProgressBar(pb, 1)


# first day of school
roster.10.2011<-dbGetQuery(pscon, "
                            Select N.LastFirst, R.StudentID, R.GRADE_LEVEL,CALENDARDATE
                            FROM (
                                  SELECT * 
                                  FROM PS_MEMBERSHIP_DEFAULTS 
                                  WHERE Calendardate='3-OCT-2011' 
                                  AND SCHOOLID = 7810
                                ) R
                          JOIN (
                               SELECT ID, LASTFIRST FROM STUDENTS
                               ) N
                          ON N.ID = R.StudentID
                          "
                          )

# Get rosters on for 2010-2011

# last day of school
roster.6.2011<-dbGetQuery(pscon, "
                          Select N.LastFirst, R.StudentID, R.GRADE_LEVEL,CALENDARDATE
                          FROM (
                          SELECT * 
                          FROM PS_MEMBERSHIP_DEFAULTS 
                          WHERE Calendardate='15-JUN-2011' 
                          AND SCHOOLID = 7810
                          ) R
                          JOIN (
                          SELECT ID, LASTFIRST FROM STUDENTS
                          ) N
                          ON N.ID = R.StudentID
                          "
)

setTxtProgressBar(pb, 2)

# first day of school
roster.10.2010<-dbGetQuery(pscon, "
                           Select N.LastFirst, R.StudentID, R.GRADE_LEVEL,CALENDARDATE
                           FROM (
                           SELECT * 
                           FROM PS_MEMBERSHIP_DEFAULTS 
                           WHERE Calendardate='1-OCT-2010' 
                           AND SCHOOLID = 7810
                           ) R
                           JOIN (
                           SELECT ID, LASTFIRST FROM STUDENTS
                           ) N
                           ON N.ID = R.StudentID
                           "
                          )
setTxtProgressBar(pb, 3)
# Get rosters on for 2009-2010

# last day of school
roster.6.2010<-dbGetQuery(pscon, "
                          Select N.LastFirst, R.StudentID, R.GRADE_LEVEL,CALENDARDATE
                          FROM (
                          SELECT * 
                          FROM PS_MEMBERSHIP_DEFAULTS 
                          WHERE Calendardate='15-JUN-2010' 
                          AND SCHOOLID = 7810
                          ) R
                          JOIN (
                          SELECT ID, LASTFIRST FROM STUDENTS
                          ) N
                          ON N.ID = R.StudentID
                          "
)
setTxtProgressBar(pb, 4)

# first day of school
roster.10.2009<-dbGetQuery(pscon, "
                           Select N.LastFirst, R.StudentID, R.GRADE_LEVEL,CALENDARDATE
                           FROM (
                           SELECT * 
                           FROM PS_MEMBERSHIP_DEFAULTS 
                           WHERE Calendardate='1-OCT-2009' 
                           AND SCHOOLID = 7810
                           ) R
                           JOIN (
                           SELECT ID, LASTFIRST FROM STUDENTS
                           ) N
                           ON N.ID = R.StudentID
                           "
                          )

setTxtProgressBar(pb, 5)

# Get rosters on for 2008-2009

# last day of school
roster.6.2009<-dbGetQuery(pscon, "
                          Select N.LastFirst, R.StudentID, R.GRADE_LEVEL,CALENDARDATE
                          FROM (
                          SELECT * 
                          FROM PS_MEMBERSHIP_DEFAULTS 
                          WHERE Calendardate='15-JUN-2009' 
                          AND SCHOOLID = 7810
                          ) R
                          JOIN (
                          SELECT ID, LASTFIRST FROM STUDENTS
                          ) N
                          ON N.ID = R.StudentID
                          "
)
setTxtProgressBar(pb, 6)

# first day of school
roster.10.2008<-dbGetQuery(pscon, "
                           Select N.LastFirst, R.StudentID, R.GRADE_LEVEL,CALENDARDATE
                           FROM (
                           SELECT * 
                           FROM PS_MEMBERSHIP_DEFAULTS 
                           WHERE Calendardate='1-OCT-2008' 
                           AND SCHOOLID = 7810
                           ) R
                           JOIN (
                           SELECT ID, LASTFIRST FROM STUDENTS
                           ) N
                           ON N.ID = R.StudentID
                           "
                          )
setTxtProgressBar(pb, 7)

# Get rosters on for 2007-2008

# last day of school
roster.6.2008<-dbGetQuery(pscon, "
                          Select N.LastFirst, R.StudentID, R.GRADE_LEVEL,CALENDARDATE
                          FROM (
                          SELECT * 
                          FROM PS_MEMBERSHIP_DEFAULTS 
                          WHERE Calendardate='13-JUN-2008' 
                          AND SCHOOLID = 7810
                          ) R
                          JOIN (
                          SELECT ID, LASTFIRST FROM STUDENTS
                          ) N
                          ON N.ID = R.StudentID
                          "
)

setTxtProgressBar(pb, 8)

# first day of school
roster.10.2007<-dbGetQuery(pscon, "
                           Select N.LastFirst, R.StudentID, R.GRADE_LEVEL,CALENDARDATE
                           FROM (
                           SELECT * 
                           FROM PS_MEMBERSHIP_DEFAULTS 
                           WHERE Calendardate='1-OCT-2007' 
                           AND SCHOOLID = 7810
                           ) R
                           JOIN (
                           SELECT ID, LASTFIRST FROM STUDENTS
                           ) N
                           ON N.ID = R.StudentID
                           "
                          )
setTxtProgressBar(pb, 9)
# Get rosters on for 2006-2007

# last day of school
roster.6.2007<-dbGetQuery(pscon, "
                          Select N.LastFirst, R.StudentID, R.GRADE_LEVEL,CALENDARDATE
                          FROM (
                          SELECT * 
                          FROM PS_MEMBERSHIP_DEFAULTS 
                          WHERE Calendardate='15-JUN-2007' 
                          AND SCHOOLID = 7810
                          ) R
                          JOIN (
                          SELECT ID, LASTFIRST FROM STUDENTS
                          ) N
                          ON N.ID = R.StudentID
                          "
)
setTxtProgressBar(pb, 10)

# first day of school
roster.10.2006<-dbGetQuery(pscon, "
                           Select N.LastFirst, R.StudentID, R.GRADE_LEVEL,CALENDARDATE
                           FROM (
                           SELECT * 
                           FROM PS_MEMBERSHIP_DEFAULTS 
                           WHERE Calendardate='2-OCT-2006' 
                           AND SCHOOLID = 7810
                           ) R
                           JOIN (
                           SELECT ID, LASTFIRST FROM STUDENTS
                           ) N
                           ON N.ID = R.StudentID
                           "
                          )
setTxtProgressBar(pb, 11)
# Get rosters on for 2005-2006

# last day of school
roster.6.2006<-dbGetQuery(pscon, "
                          Select N.LastFirst, R.StudentID, R.GRADE_LEVEL,CALENDARDATE
                          FROM (
                          SELECT * 
                          FROM PS_MEMBERSHIP_DEFAULTS 
                          WHERE Calendardate='15-JUN-2006' 
                          AND SCHOOLID = 7810
                          ) R
                          JOIN (
                          SELECT ID, LASTFIRST FROM STUDENTS
                          ) N
                          ON N.ID = R.StudentID
                          "
)

setTxtProgressBar(pb, 12)
# first day of school
roster.10.2005<-dbGetQuery(pscon, "
                           Select N.LastFirst, R.StudentID, R.GRADE_LEVEL,CALENDARDATE
                           FROM (
                           SELECT * 
                           FROM PS_MEMBERSHIP_DEFAULTS 
                           WHERE Calendardate='3-OCT-2005' 
                           AND SCHOOLID = 7810
                           ) R
                           JOIN (
                           SELECT ID, LASTFIRST FROM STUDENTS
                           ) N
                           ON N.ID = R.StudentID
                           "
                          )
setTxtProgressBar(pb, 13)
# Combine all roster files into one table

roster.kams<-rbind(roster.10.2005, roster.6.2006, roster.10.2006, roster.6.2007, roster.10.2007, roster.6.2008,roster.10.2008, roster.6.2009,roster.10.2009, roster.6.2010, roster.10.2010, roster.6.2011,roster.10.2011, roster.6.2012)
setTxtProgressBar(pb, 14)
#remove working tables
rm(roster.10.2005, roster.6.2006, roster.10.2006, roster.6.2007, roster.10.2007, roster.6.2008,roster.10.2008, roster.6.2009,roster.10.2009, roster.6.2010, roster.10.2010, roster.6.2011,roster.10.2011, roster.6.2012)

setTxtProgressBar(pb, 15)
#remove  database connections
rm(drvr, pscon)
setTxtProgressBar(pb, 16)

# close and remove progress bar
close(pb)
rm(pb)