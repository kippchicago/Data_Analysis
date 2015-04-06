source('data//map_all_silo.R')

data(ex_CombinedStudentsBySchool)
data(ex_CombinedAssessmentResults)

sel_names<-names(ex_CombinedStudentsBySchool)

sel_cdf_names <- names(ex_CombinedAssessmentResults)
# Drop DisctrictName
sel_names <- sel_names[sel_names!="DistrictName"]

map_all_silo <- map_all_silo %>%
  mutate(Grade=as.integer(ifelse(Grade=="K", "0", Grade)))


map_fall_equate<-map_all_silo %>% 
  filter(TermName=="Fall 2014-2015",
         MeasurementScale %in% c("Reading", "Mathematics")) %>%
  anti_join(map_all_silo %>% 
              filter(TermName=="Spring 2013-2014"),
            by=c("StudentID", "MeasurementScale")) %>%
mutate(TestRITScore=round(sqrpr::cps_equate(rit_score = TestRITScore, 
                                         subject = MeasurementScale,
                                         grade_level = Grade)),
       TermName="Spring 2013-2014",
       Grade=Grade-1, 
       Equated=TRUE
       ) %>% 
  filter(Grade!=-1)

map_all_silo_equate <- rbind_list(map_all_silo, map_fall_equate) %>%
  mutate(TestID=ifelse(is.na(TestID), paste(StudentID, TermName, MeasurementScale), TestID))

map_silo <- map_all_silo_equate %>% filter(TermName %in% c("Spring 2013-2014",
                                                    "Winter 2014-2015"),
                                    MeasurementScale %in% c("Reading",
                                                            "Mathematics"),
                                    !is.na(TestID),
                                    GrowthMeasureYN=="TRUE"
) %>% mutate(StudentID=as.character(StudentID),
             GrowthMeasureYN=as.logical(GrowthMeasureYN),
             TestDurationMinutes=as.integer(TestDurationMinutes),
             TestRITScore = as.integer(TestRITScore),
             TestPercentile = as.integer(TestPercentile),
             PercentCorrect = as.integer(PercentCorrect)
)

map_students <- map_silo[, c(sel_names, "MeasurementScale", "TestPercentile")] %>%
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

# Bloom ####
# Amy's list ####


growth_stus<-map_mv$growth_df %>%
  inner_join(map_mv$roster %>%
               filter(termname=="Winter 2014-2015"), 
             by=c("studentid",
                  "measurementscale")
             ) %>%
  filter(grepl("Bloom", schoolname)) %>%
  mutate(GrowthType=ifelse(rit_growth<0, "Negative", "Not Typical"),
         GrowthType=ifelse(met_typical_growth, "Typical", GrowthType),
         GrowthType=ifelse(met_accel_growth, "College Ready", GrowthType),
         GrowthType=factor(GrowthType, levels=c("Negative", 
                                                "Not Typical",
                                                "Typical",
                                                "College Ready"))
         
  ) %>%
  group_by(end_grade, measurementscale, GrowthType) %>%
mutate(GrowthTypeRank = row_number(end_testritscore),
       stu_plot_name=sprintf("%s %s (%s / %s pctl)",
                             studentfirstname,
                             studentlastname,
                             end_testritscore,
                             toOrdinal::toOrdinal(end_testpercentile))
)

growth_stus_summary <- growth_stus %>%
  group_by(end_grade, measurementscale, GrowthType) %>%
  summarize(N=n()) %>%
  group_by(end_grade, measurementscale) %>%
            mutate(Tot=sum(N),
                   Pct=N/Tot,
                   loc=ifelse(GrowthType=="College Ready",40,37.5),
                   loc=ifelse(GrowthType=="Not Typical",35,loc),
                   loc=ifelse(GrowthType=="Negative",32.5,loc),
                   Text=paste("% ", GrowthType, " = ", 
                               round(Pct*100), "% (", N,"/", Tot, ")", 
                               sep="")
                   )



# Amy's list of lists ####


grades<-unique(as.integer(growth_stus$end_grade))
p<-list()
for (g in grades){
  message(sprintf("Working on grade %s", g))
  p[[as.character(g)]] <- ggplot(growth_stus %>% filter(end_grade==g), 
aes(x=GrowthType, y=GrowthTypeRank)) +
    geom_text(aes(label=stu_plot_name, color=GrowthType), size=1.75) + 
    geom_text(data=growth_stus_summary %>% filter(end_grade==g), 
              aes(x="Not Typical", y=loc, label=Text, color=GrowthType),
              size=3,
              hjust=0
    ) +
    scale_color_manual(values=c("red",
                                "#C49A6C",
                                "#8D8685",
                                "#FEBC11")) +
    facet_grid(end_grade ~ measurementscale) + 
    theme_bw() + theme(legend.position="bottom") +
    xlab("Growth Type") + 
    ylab("Count of Students")
}


pdf(file="graphs/KBCP_Amys_List.pdf", onefile = TRUE, width=11, heigh=8.5)
  p
dev.off()



# Create ####
# Amy's list ####


growth_stus<-map_mv$growth_df %>%
  inner_join(map_mv$roster %>%
               filter(termname=="Winter 2014-2015"), 
             by=c("studentid",
                  "measurementscale")
  ) %>%
  filter(grepl("Create", schoolname)) %>%
  mutate(GrowthType=ifelse(rit_growth<0, "Negative", "Not Typical"),
         GrowthType=ifelse(met_typical_growth, "Typical", GrowthType),
         GrowthType=ifelse(met_accel_growth, "College Ready", GrowthType),
         GrowthType=factor(GrowthType, levels=c("Negative", 
                                                "Not Typical",
                                                "Typical",
                                                "College Ready"))
         
  ) %>%
  group_by(end_grade, measurementscale, GrowthType) %>%
  mutate(GrowthTypeRank = row_number(end_testritscore),
         stu_plot_name=sprintf("%s %s (%s / %s pctl)",
                               studentfirstname,
                               studentlastname,
                               end_testritscore,
                               toOrdinal::toOrdinal(end_testpercentile))
  )

growth_stus_summary <- growth_stus %>%
  group_by(end_grade, measurementscale, GrowthType) %>%
  summarize(N=n()) %>%
  group_by(end_grade, measurementscale) %>%
  mutate(Tot=sum(N),
         Pct=N/Tot,
         loc=ifelse(GrowthType=="College Ready",40,37.5),
         loc=ifelse(GrowthType=="Not Typical",35,loc),
         loc=ifelse(GrowthType=="Negative",32.5,loc),
         Text=paste("% ", GrowthType, " = ", 
                    round(Pct*100), "% (", N,"/", Tot, ")", 
                    sep="")
  )



# Amy's list of lists ####


grades<-unique(as.integer(growth_stus$end_grade))
p<-list()
for (g in grades){
  message(sprintf("Working on grade %s", g))
  p[[as.character(g)]] <- ggplot(growth_stus %>% filter(end_grade==g), 
                                 aes(x=GrowthType, y=GrowthTypeRank)) +
    geom_text(aes(label=stu_plot_name, color=GrowthType), size=1.75) + 
    geom_text(data=growth_stus_summary %>% filter(end_grade==g), 
              aes(x="Not Typical", y=loc, label=Text, color=GrowthType),
              size=3,
              hjust=0
    ) +
    scale_color_manual(values=c("red",
                                "#C49A6C",
                                "#8D8685",
                                "#FEBC11")) +
    facet_grid(end_grade ~ measurementscale) + 
    theme_bw() + theme(legend.position="bottom") +
    xlab("Growth Type") + 
    ylab("Count of Students")
}


pdf(file="graphs/KCCP_Amys_List.pdf", onefile = TRUE, width=11, heigh=8.5)
p
dev.off()


# Create ####
# Amy's list ####


growth_stus<-map_mv$growth_df %>%
  inner_join(map_mv$roster %>%
               filter(termname=="Winter 2014-2015"), 
             by=c("studentid",
                  "measurementscale")
  ) %>%
  filter(grepl("Ascend", schoolname)) %>%
  mutate(GrowthType=ifelse(rit_growth<0, "Negative", "Not Typical"),
         GrowthType=ifelse(met_typical_growth, "Typical", GrowthType),
         GrowthType=ifelse(met_accel_growth, "College Ready", GrowthType),
         GrowthType=factor(GrowthType, levels=c("Negative", 
                                                "Not Typical",
                                                "Typical",
                                                "College Ready"))
         
  ) %>%
  group_by(end_grade, measurementscale, GrowthType) %>%
  mutate(GrowthTypeRank = row_number(end_testritscore),
         stu_plot_name=sprintf("%s %s (%s / %s pctl)",
                               studentfirstname,
                               studentlastname,
                               end_testritscore,
                               toOrdinal::toOrdinal(end_testpercentile))
  )

growth_stus_summary <- growth_stus %>%
  group_by(end_grade, measurementscale, GrowthType) %>%
  summarize(N=n()) %>%
  group_by(end_grade, measurementscale) %>%
  mutate(Tot=sum(N),
         Pct=N/Tot,
         loc=ifelse(GrowthType=="College Ready",40,37.5),
         loc=ifelse(GrowthType=="Not Typical",35,loc),
         loc=ifelse(GrowthType=="Negative",32.5,loc),
         Text=paste("% ", GrowthType, " = ", 
                    round(Pct*100), "% (", N,"/", Tot, ")", 
                    sep="")
  )



# Amy's list of lists ####


grades<-unique(as.integer(growth_stus$end_grade))
p<-list()
for (g in grades){
  message(sprintf("Working on grade %s", g))
  p[[as.character(g)]] <- ggplot(growth_stus %>% filter(end_grade==g), 
                                 aes(x=GrowthType, y=GrowthTypeRank)) +
    geom_text(aes(label=stu_plot_name, color=GrowthType), size=1.75) + 
    geom_text(data=growth_stus_summary %>% filter(end_grade==g), 
              aes(x="Not Typical", y=loc, label=Text, color=GrowthType),
              size=3,
              hjust=0
    ) +
    scale_color_manual(values=c("red",
                                "#C49A6C",
                                "#8D8685",
                                "#FEBC11")) +
    facet_grid(end_grade ~ measurementscale) + 
    theme_bw() + theme(legend.position="bottom") +
    xlab("Growth Type") + 
    ylab("Count of Students")
}


pdf(file="graphs/KACP_Amys_List.pdf", onefile = TRUE, width=11, heigh=8.5)
p
dev.off()


# CSVs ####
write.csv(growth_stus %>%
            select(studentid, 
                   studentlastname,
                   studentfirstname,
                   school,
                   end_grade,
                   growth_window,
                   start_season=start_fallwinterspring,
                   end_seson=end_fallwinterspring,
                   start_testritscore,
                   end_testritscore,
                   start_testpercentile,
                   end_testpercentile,
                   expected_growth=reported_growth,
                   actual_growth = rit_growth,
                   change_testpercentile,
                   met_typical_growth,
                   cr_growth=accel_growth,
                   met_cr_growth=met_accel_growth,
                   growth_type=GrowthType
                   ) %>% ungroup %>%
            arrange(end_grade, 
                    measurementscale, 
                    studentlastname, 
                    studentfirstname)
          , "reports/KBCP_Amys_List.csv")
