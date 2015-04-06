#### Growth percentages from last year

#load.project()


data(ex_CombinedStudentsBySchool)
data(ex_CombinedAssessmentResults)

sel_names<-names(ex_CombinedStudentsBySchool)

sel_cdf_names <- names(ex_CombinedAssessmentResults)
# Drop DisctrictName
sel_names <- sel_names[sel_names!="DistrictName"]

map_silo <- map_all_silo %>% filter(TermName %in% c("Spring 2012-2013",
                                                    "Winter 2013-2014",
                                                    "Spring 2013-2014",
                                                    "Winter 2014-2015"),
                                    MeasurementScale %in% c("Reading",
                                                             "Mathematics"),
                                    !is.na(TestID),
                                    GrowthMeasureYN=="TRUE",
                                    Tested_at_KIPP=="TRUE"
                                    ) %>% mutate(StudentID=as.character(StudentID),
             GrowthMeasureYN=as.logical(GrowthMeasureYN),
             TestDurationMinutes=as.integer(TestDurationMinutes),
             TestRITScore = as.integer(TestRITScore),
             TestPercentile = as.integer(TestPercentile),
             PercentCorrect = as.integer(PercentCorrect)
             )

map_students <- map_silo[, c(sel_names, "MeasurementScale", "TestPercentile")] %>%
  filter(MeasurementScale=="Reading") %>%
  mutate(school=abbrev(SchoolName, exceptions = list(old=c("KAPS", "KAMS"),
                                                     new=c("KACP", "KACP")
                                                     )
                       ),
         TestQuartile=kipp_quartile(x=TestPercentile)
         ) %>%
  unique %>%
  as.data.frame

current_students<-map_students %>% 
  dplyr::filter(TermName=="Winter 2014-2015") %>%
  dplyr::select(studentid=StudentID, school, Grade) %>%
  unique




map_cdf <- map_silo %>%
  select(-StudentLastName:-Grade) %>%
  as.data.frame

map_mv<-mapvizieR(cdf = map_cdf, 
                  roster = map_students,
                  include_unsanctioned_windows=TRUE
                  )

growth<-map_mv$growth_df


growth_summary<-growth %>% dplyr::filter(complete_obsv) %>%
  dplyr::group_by(end_grade, end_schoolname, measurementscale, growth_window, end_map_year_academic) %>%
  dplyr::summarize(N=n(),
                   start_avg_rit=mean(start_testritscore),
                   end_avg_rit=mean(end_testritscore),
                   avg_growth=mean(rit_growth)
  ) %>%
  dplyr::filter(N>=60) %>%
  dplyr::ungroup()


map_growth_sws <- dplyr::left_join(growth_summary %>%  dplyr::filter(growth_window=="Spring to Winter"),
                            growth_summary %>%  dplyr::filter(growth_window=="Winter to Spring"),
                            by=c("end_map_year_academic",
                                 "end_grade",
                                 "end_schoolname",
                                 "measurementscale")) %>%
  dplyr::mutate(mult=(avg_growth.y+avg_growth.x)/avg_growth.x,
                School=abbrev(end_schoolname)
  )

names(map_growth_sws) <- str_replace(colnames(map_growth_sws), "\\.x", "_sw")
names(map_growth_sws) <- str_replace(colnames(map_growth_sws), "\\.y", "_ws")

#m.table<-m.fws.s[N.fw>20, list(School, MeasurementScale, Grade, "FW RIT Growth"=diff.fw, "WS RIT Growth"=diff.ws, "Multiplier (WS/FW)=mult)]
map_pdata<-map_growth_sws %>%
  dplyr::select(School,
                Year=end_map_year_academic,
                Subject=measurementscale, 
                Grade=end_grade, 
                growth_window_sw,
                growth_window_ws,
                avg_growth_sw, 
                avg_growth_ws, 
                mult
                ) %>%
  dplyr::mutate(Year=Year+1)
