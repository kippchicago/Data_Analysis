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

map.F13.kaps[,CollegeReadyGrowth_Chi:=round(
  TypicalFallToSpringGrowth + sigma*SDFallToSpringGrowth,0)]

# Create new college ready growth targets based on Foundation  

# Calculate Quartiles from Percentiles for each students
map.F13.kaps[TestPercentile<25 ,TestQuartile:=1]
map.F13.kaps[TestPercentile >= 25 & TestPercentile <50 ,TestQuartile:=2]
map.F13.kaps[TestPercentile >= 50 & TestPercentile <75 ,TestQuartile:=3]
map.F13.kaps[TestPercentile >= 75 ,TestQuartile:=4]

#Lookup Tiered Multiplier
map.F13.kaps[TestQuartile == 1 & Grade <= 3, TieredMultiplier:=1.5]
map.F13.kaps[TestQuartile == 2 & Grade <= 3, TieredMultiplier:=1.5]
map.F13.kaps[TestQuartile == 3 & Grade <= 3, TieredMultiplier:=1.25]
map.F13.kaps[TestQuartile == 4 & Grade <= 3, TieredMultiplier:=1.25]

map.F13.kaps[TestQuartile == 1 & Grade > 3, TieredMultiplier:=2]
map.F13.kaps[TestQuartile == 2 & Grade > 3, TieredMultiplier:=1.75]
map.F13.kaps[TestQuartile == 3 & Grade > 3, TieredMultiplier:=1.5]
map.F13.kaps[TestQuartile == 4 & Grade > 3, TieredMultiplier:=1.25]

#Calculate new College Ready Growth and College Ready Target
map.F13.kaps[,CollegeReadyGrowth:=round(TypicalFallToSpringGrowth*TieredMultiplier,0)]


##### Winter 13 ####
# Strip leading white space from SchoolName field
map.W13[,SchoolName:=str_trim(SchoolName, side="left")]

#subset to only KAPS
map.W13.kaps<-copy(map.W13[SchoolName=="KIPP Ascend Primary School" & Grade<=3])

map.S13W13.kaps<-copy(map.S13W13[SchoolName=="KIPP Ascend Primary" & Fall13_Grade<=3])


map.W13.kaps<-copy(map.W13.kaps[,list(StudentID, 
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

map.W13.kaps[,CollegeReadyGrowth_Chi:=round(
  TypicalFallToSpringGrowth + sigma*SDFallToSpringGrowth,0)]

# Create new college ready growth targets based on Foundation  

# Calculate Quartiles from Percentiles for each students
map.W13.kaps[TestPercentile<25 ,TestQuartile:=1]
map.W13.kaps[TestPercentile >= 25 & TestPercentile <50 ,TestQuartile:=2]
map.W13.kaps[TestPercentile >= 50 & TestPercentile <75 ,TestQuartile:=3]
map.W13.kaps[TestPercentile >= 75 ,TestQuartile:=4]

#Lookup Tiered Multiplier
map.W13.kaps[TestQuartile == 1 & Grade <= 3, TieredMultiplier:=1.5]
map.W13.kaps[TestQuartile == 2 & Grade <= 3, TieredMultiplier:=1.5]
map.W13.kaps[TestQuartile == 3 & Grade <= 3, TieredMultiplier:=1.25]
map.W13.kaps[TestQuartile == 4 & Grade <= 3, TieredMultiplier:=1.25]

map.W13.kaps[TestQuartile == 1 & Grade > 3, TieredMultiplier:=2]
map.W13.kaps[TestQuartile == 2 & Grade > 3, TieredMultiplier:=1.75]
map.W13.kaps[TestQuartile == 3 & Grade > 3, TieredMultiplier:=1.5]
map.W13.kaps[TestQuartile == 4 & Grade > 3, TieredMultiplier:=1.25]

#Calculate new College Ready Growth and College Ready Target
map.W13.kaps[,CollegeReadyGrowth:=round(TypicalFallToSpringGrowth*TieredMultiplier,0)]