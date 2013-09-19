# Munging Script

# Strip leading white space from SchoolName field
map.F13[,SchoolName:=str_trim(SchoolName, side="left")]

#subset to only KAPS
map.F13.kaps<-copy(map.F13[SchoolName=="KIPP Ascend Primary School" & Grade<=3])

map.S13F13.kaps<-copy(map.S13F13[SchoolName=="KIPP Ascend Primary" & Fall13_Grade<=3])


map.F13.kaps<-copy(map.F13.kaps[,list(StudentID, 
                StudentLastName, 
                StudentFirstName, 
                Grade, 
                ClassName,
                TeacherName,                    
                MeasurementScale,
                TestRITScore,
                TestPercentile,                                      
                ReportedFallToWinterGrowth,
                TypicalFallToSpringGrowth,                      
                ReportedFallToSpringGrowth,
                SDFallToSpringGrowth
                ), ])

sigma<-qnorm(.75)

map.F13.kaps[,CollegeReadyGrowth:=round(TypicalFallToSpringGrowth + sigma*SDFallToSpringGrowth,0)]


