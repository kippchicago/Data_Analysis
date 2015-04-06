library(ProjectTemplate)

load.project()


#set keys
setkey(map.F13.kaps, StudentID, MeasurementScale)

map.S13.kaps<-copy(map.S13F13.kaps[,list(ID, Subject, Spring13_RIT, ReportedSpringToSpringGrowth)])

setkey(map.S13.kaps, ID, Subject)

#merge spring 13 adn fall 13 data so we can get Spring to Spring Goals
csv.map.F13.kaps<-copy(map.S13.kaps[map.F13.kaps])

csv.map.F13.kaps[,c("TypicalFallToSpringGrowth",
                    "SDFallToSpringGrowth") := NULL]


#Create End of Year Targets
csv.map.F13.kaps[,GoalTypicalFW:=TestRITScore + ReportedFallToWinterGrowth]
csv.map.F13.kaps[,GoalTypicalFS:=TestRITScore + ReportedFallToSpringGrowth]
csv.map.F13.kaps[,GoalCollegeReadyFS:=TestRITScore + CollegeReadyGrowth]
csv.map.F13.kaps[,GoalCollegeReadyFS_Chi:=TestRITScore + CollegeReadyGrowth_Chi]
csv.map.F13.kaps[,GoalPlatSS:=Spring13_RIT + ReportedSpringToSpringGrowth]


setcolorder(csv.map.F13.kaps, c(1,5:9,2,10:11,15,3,14,17:22,4,12:13,16))


setnames(csv.map.F13.kaps, c("ID", 
                             "TestRITScore", 
                             "TestPercentile",
                             "TestQuartile",
                             "Spring13_RIT",   
                             "ReportedFallToWinterGrowth",
                             "ReportedFallToSpringGrowth",
                             "CollegeReadyGrowth_Chi",
                             "CollegeReadyGrowth",
                             "GoalTypicalFW",
                             "GoalTypicalFS",
                             "GoalCollegeReadyFS",
                             "GoalCollegeReadyFS_Chi",
                             "GoalPlatSS"   
                             ),
                          c("Student Number",
                            "Fall 13 RIT Score",
                            "Fall 13 NPR",
                            "Fall 13 Quartile",
                            "Spring 13 RIT Score",
                       "Typical F-to-W Growth (MOY)",
                        "Typical F-to-S Growth (EOY Silver)",
                            "OLD College Ready Growth (OLD EOY Gold)",
                        "College Ready Growth (EOY Gold)",
                         "RIT Typical F-to-W Goal (MOY)",
                         "RIT Typical F-to-S Goal (EOY Silver)",
                         "RIT College Ready F-to-S Goal (EOY Gold)",
                        "RIT OLD College Ready F-to-S Goal (OLD EOY Gold)",   
                         "RIT Typical S-to-S Goal (EOY Platinum)")
      )

csv.map.F13.kaps[,c("ReportedSpringToSpringGrowth", "TieredMultiplier") := NULL]

write.csv(csv.map.F13.kaps[order(Grade, 
                                 TeacherName,
                                 StudentLastName, 
                                 StudentFirstName, 
                                 Subject)],
          file='reports/MAP_Fall_13_KAPS_3.csv')
