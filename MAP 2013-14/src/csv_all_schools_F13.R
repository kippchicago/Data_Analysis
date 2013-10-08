library(ProjectTemplate)

load.project()


#set keys
setkey(map.F13.working, StudentID, MeasurementScale)

map.S13.working<-copy(map.S13F13[,list(ID, Subject, Spring13_RIT, ReportedSpringToSpringGrowth)])

setkey(map.S13.working, ID, Subject)

#merge spring 13 adn fall 13 data so we can get Spring to Spring Goals
csv.map.F13.working<-copy(map.S13.working[map.F13.working])

csv.map.F13.working[,c("TypicalFallToSpringGrowth",
                    "SDFallToSpringGrowth") := NULL]


#Create End of Year Targets
csv.map.F13.working[,GoalTypicalFW:=TestRITScore + ReportedFallToWinterGrowth]
csv.map.F13.working[,GoalTypicalFS:=TestRITScore + ReportedFallToSpringGrowth]
csv.map.F13.working[,GoalCollegeReadyFS:=TestRITScore + CollegeReadyGrowth]
csv.map.F13.working[,GoalCollegeReadyFS_Chi:=TestRITScore + CollegeReadyGrowth_Chi]
csv.map.F13.working[,GoalPlatSS:=Spring13_RIT + ReportedSpringToSpringGrowth]


setcolorder(csv.map.F13.working, 
            c("SchoolName",
              "ID",
              "StudentLastName",
              "StudentFirstName",
              "Grade",
              "ClassName",
              "TeacherName",
              "Subject",
              "TestRITScore",
              "TestPercentile",
              "TestQuartile",
              "TieredMultiplier",
              "Spring13_RIT",
              "CollegeReadyGrowth_Chi",
              "CollegeReadyGrowth",
              "GoalTypicalFW",
              "GoalTypicalFS",
              "GoalCollegeReadyFS",
              "GoalCollegeReadyFS_Chi",
              "GoalPlatSS",
              "ReportedFallToWinterGrowth",
              "ReportedFallToSpringGrowth",
              "ReportedSpringToSpringGrowth"
              )
)
            




setnames(csv.map.F13.working, c("ID", 
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
                             "GoalPlatSS",
                             "ReportedSpringToSpringGrowth",
                              "TieredMultiplier"
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
                         "RIT Typical S-to-S Goal (EOY Platinum)",
                            "Platinum College Ready Growth (EOY Platinum)",
                            "Tiered Growth Multiplier")
      )

csv.map.F13.working[,c("ReportedSpringToSpringGrowth", "TieredMultiplier") := NULL]

write.csv(csv.map.F13.working[][order(Grade, 
                                 TeacherName,
                                 StudentLastName, 
                                 StudentFirstName, 
                                 Subject)],
          file='reports/MAP_Fall_13_KAPS_3.csv')
