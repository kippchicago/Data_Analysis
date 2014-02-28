s2s_match <- function(.data, season1="Fall", season2="Spring", sy=2013){
  
  # Filter to Season1
  m.1<-filter(.data, Season==season1, Year2==sy)
  
  # Filter to Season2
  m.2<-filter(.data, Season==season2, Year2==sy)
  
  
  # Join on ID and MeasurementScale
  m.12<-inner_join(m.1, m.2, by=c("StudentID", "MeasurementScale"))
  
  # construct and substitute names
  typical.growth <- as.name(paste0("Typical", season1, "To", season2, "Growth.x"))
  q<-substitute(typical.growth + TestRITScore.x)
  
  season.growth <- as.name(paste0(season1, "To", season2, "Growth.x"))
  q<-substitute(typical.growth + TestRITScore.x)
  
  m.12<-with(m.12, mutate(m.12, ProjectedGrowth=eval(q), MetTypical=TestRITScore.y>=ProjectedGrowth, GrowthSeason=paste(Season.x, Season.y, sep=" - ")))
  
  #return
  m.12
}

s2s_summary<-function(.data, school="KCCP"){

.data[MeasurementScale %in% c("Reading", "Mathematics"),
     list(N=.N, 
          avg_fall=mean(TestRITScore.x), 
          avg_winter=mean(TestRITScore.y), 
          growth2 = mean(((TestRITScore.y-TestRITScore.x)*2)+TestRITScore.x), 
          growth1.5 = mean(((TestRITScore.y-TestRITScore.x)*1.5)+TestRITScore.x)),    
     keyby=list(SchoolInitials.x, 
             MeasurementScale, 
             Grade.x)][SchoolInitials.x %in% school][order(MeasurementScale, Grade.x),]

}

attainment_summary <-  function(.data, 
                                year=2014, 
                                season="Fall", 
                                school="KCCP"){
  .data[MeasurementScale %in% c("Reading", "Mathematics") 
        & Season %in% season 
        & Year2 %in% year 
        & SchoolInitials %in% school,
        list(N=.N,
             avg_attainment=mean(TestRITScore)
             ),
        keyby=list(SchoolInitials, MeasurementScale, Grade)][order(MeasurementScale, Grade)]
  
}

pct_me_growth <-function(.data, school){
  .data[MeasurementScale %in% c("Reading", "Mathematics") 
       & SchoolInitials.x %in% school,
       list(N=.N, PCt=sum(MetTypical)/.N)]
  
}