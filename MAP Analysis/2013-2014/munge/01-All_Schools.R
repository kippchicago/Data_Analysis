# Munging Script

# Strip leading white space from SchoolName field
map.F13[,SchoolName:=str_trim(SchoolName, side="left")]

#subset to only KAPS
#map.F13.working<-copy(map.F13[SchoolName=="KIPP Ascend Primary School" & Grade<=3])

#map.S13F13.kaps<-copy(map.S13F13[SchoolName=="KIPP Ascend Primary" & Fall13_Grade<=3])


map.F13.working<-copy(map.F13[,list(SchoolName,
                                      StudentID, 
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
                                      SDFallToSpringGrowth,
                                      RITtoReadingScore
), ])

sigma<-qnorm(.75)

map.F13.working[,CollegeReadyGrowth_Chi:=round(TypicalFallToSpringGrowth + sigma*SDFallToSpringGrowth,0)]

# Create new college ready growth targets based on Foundation  

# Calculate Quartiles from Percentiles for each students
map.F13.working[TestPercentile<25 ,TestQuartile:=1]
map.F13.working[TestPercentile >= 25 & TestPercentile <50 ,TestQuartile:=2]
map.F13.working[TestPercentile >= 50 & TestPercentile <75 ,TestQuartile:=3]
map.F13.working[TestPercentile >= 75 ,TestQuartile:=4]

#Lookup Tiered Multiplier
map.F13.working[TestQuartile == 1 & Grade <= 3, TieredMultiplier:=1.5]
map.F13.working[TestQuartile == 2 & Grade <= 3, TieredMultiplier:=1.5]
map.F13.working[TestQuartile == 3 & Grade <= 3, TieredMultiplier:=1.25]
map.F13.working[TestQuartile == 4 & Grade <= 3, TieredMultiplier:=1.25]

map.F13.working[TestQuartile == 1 & Grade > 3, TieredMultiplier:=2]
map.F13.working[TestQuartile == 2 & Grade > 3, TieredMultiplier:=1.75]
map.F13.working[TestQuartile == 3 & Grade > 3, TieredMultiplier:=1.5]
map.F13.working[TestQuartile == 4 & Grade > 3, TieredMultiplier:=1.25]

#Calculate new College Ready Growth and College Ready Target
map.F13.working[,CollegeReadyGrowth:=round(TypicalFallToSpringGrowth*TieredMultiplier,0)]