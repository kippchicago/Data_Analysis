library(ProjectTemplate)

load.project()


#set keys
setkey(map.F13.kaps, StudentID, MeasurementScale)

map.S13.kaps<-copy(map.S13F13.kaps[,list(ID, Subject, Spring13_RIT, ReportedSpringToSpringGrowth)])

setkey(map.S13.kaps, ID, Subject)

#merge spring 13 adn fall 13 data so we can get Spring to Spring Goals
csv.map.F13.kaps<-copy(map.S13.kaps[map.F13.kaps])

csv.map.F13.kaps[,c("TypicalFallToSpringGrowth","SDFallToSpringGrowth") := NULL]


#Create End of Year Targets
csv.map.F13.kaps[,GoalTypicalFW:=TestRITScore + ReportedFallToWinterGrowth]
csv.map.F13.kaps[,GoalTypicalFS:=TestRITScore + ReportedFallToSpringGrowth]
csv.map.F13.kaps[,GoalCollegeRreadyFS:=TestRITScore + CollegeReadyGrowth]
csv.map.F13.kaps[,GoalPlatSS:=Spring13_RIT + ReportedSpringToSpringGrowth]

setcolorder(csv.map.F13.kaps, c(1,5:9,2,10:11,3,12:18,4))


setnames(csv.map.F13.kaps, c("ID", 
                             "TestRITScore", 
                             "TestPercentile",
                             "Spring13_RIT",   
                             "ReportedFallToWinterGrowth",
                             "ReportedFallToSpringGrowth",
                             "CollegeReadyGrowth",
                             "GoalTypicalFW",
                             "GoalTypicalFS",
                             "GoalCollegeRreadyFS",
                             "GoalPlatSS"   
                             ),
                          c("Student Number",
                            "Fall 13 RIT Score",
                            "Fall 13 NPR",
                            "Spring 13 RIT Score",
                       "Typical F-to-W Growth (MOY)",
                        "Typical F-to-S Growth (EOY Silver)",
                   "College Ready Growth (EOY Gold)",
                         "RIT Typical F-to-W Goal (MOY)",
                         "RIT Typical F-to-S Goal (EOY Silver)",
                         "RIT College Ready F-to-S Goal (EOY Gold)",
                         "RIT Typical S-to-S Goal (EOY Platinum)")
      )

csv.map.F13.kaps[,ReportedSpringToSpringGrowth := NULL]

write.csv(csv.map.F13.kaps[order(Grade, 
                                 TeacherName,
                                 StudentLastName, 
                                 StudentFirstName, 
                                 Subject)],
          file='reports/MAP_Fall_13_KAPS_2.csv')
