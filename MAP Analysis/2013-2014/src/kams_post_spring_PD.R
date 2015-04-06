require(dplyr)
require(mapvisuals)
require(scales)


# Get classroom assignements ####
map.db<-as.data.frame(read.dcf('config//mapdb.dcf'))

mapsrc<-src_mysql(dbname=as.character(map.db$dbname), 
                  host=as.character(map.db$host), 
                  user=as.character(map.db$user), 
                  password=as.character(map.db$password))

# get 
class.assignments<-collect(tbl(mapsrc, "tblClassAssignmentsSpring14"))



# Generating Andrew's Rainbow background ####

blank_math_template <- rit_height_weight_ACT(
  desired_subj='Math'
  ,localization=localize('Chicago')
  ,annotation_style='small numbers'
  ,line_style='gray lines'
)

blank_reading_template <- rit_height_weight_ACT(
  desired_subj='Reading'
  ,localization=localize('Chicago')
  ,annotation_style='small numbers'
  ,line_style='gray lines'
)

png("graphs/KAP_long_RIT.png", width = 10, height=10, units = "in", res=125)
  build_student_college_plot(blank_math_template, tdm, "Math", 1, localization = localize('Chicago'))
dev.off()


# Subset to classes of 2024 and 2025
kams.map.1<-filter(map.all, 
                SchoolInitials=="KAMS",
                CohortYear<=2021) %>% 
  mutate(grade_level_x=ifelse(Season=="Fall", 
                     Grade-.8, 
                     ifelse(Season=="Winter", 
                            Grade-.5, 
                            Grade
                            )
                     ),
         testritscore=TestRITScore,
         test_label=paste(Season, testritscore, sep=":\n")
         ) 


# Get unique id's for kids that tested in this last term (as proxy for current roster)

current.students<-filter(map.all, Season=="Spring", Year2==2014, SchoolInitials=="KAMS", Grade>=5) %.%
  select(StudentID) %>% summarise(StudentID=unique(StudentID))
  
kams.map<-inner_join(current.students, kams.map.1, by="StudentID")



blank_math_template <- rit_height_weight_ACT(
  desired_subj='Math'
  ,localization=localize('Chicago')
  ,annotation_style='small numbers'
  ,line_style='gray lines'
  ,school_type='ES'
)

blank_reading_template <- rit_height_weight_ACT(
  desired_subj='Reading'
  ,localization=localize('Chicago')
  ,annotation_style='small numbers'
  ,line_style='gray lines',
  ,school_type='ES'
)

kams_long<-function(.data, ID, math_temp, read_temp, labels_grade=6){
  
 

  student.df<-filter(.data, StudentID==ID)
  n<-nrow(student.df)
  student.name<-paste(student.df$StudentLastname[n], 
                      student.df$StudentFirstname[n], 
                      sep=", ")
  student.grade <- max(student.df$Grade)
#  student.class <- student.df$ClassName[n]

p.math<-build_student_college_plot(base_plot = math_temp, 
                                   stu_map_df = filter(student.df,
                                                       MeasurementScale=="Mathematics"
                                                       ), 
                                   desired_subj = "Math", 
                                   labels_at_grade = student.grade, 
                                   localization = localize('Chicago'),
                                   aspect_ratio=.6) +
  ggtitle("Math")

p.read<-build_student_college_plot(base_plot = read_temp, 
                                   stu_map_df = filter(student.df, 
                                                       MeasurementScale=="Reading"
                                   ), 
                                   desired_subj = "Reading", 
                                   labels_at_grade = student.grade, 
                                   localization = localize('Chicago'),
                                   aspect_ratio=.6) + 
  ggtitle("Reading")

titl<-paste0("\n Student: ", student.name, "\n ",
             #"Classroom: ", student.class, "\n ",
              "Grade: ", student.grade)
message(paste0("Writing Andrew Rainbow Longitudinal Visualization for: \n",
              titl)
        )
grid.arrange(p.math, p.read, nrow=2, main = titl)

}

kams.map <- arrange(kams.map, 
                   Grade, 
                   StudentLastname, 
                   StudentFirstname, 
                   MeasurementScale
                   )
students<-unique(kams.map$StudentID)
pdf("graphs/KAMS_long_RIT_all_students.pdf", width = 8.5, height = 11)
    lapply(students, 
           kams_long, 
           .data=kams.map, 
           math_temp=blank_math_template, 
           read_temp=blank_reading_template)
dev.off()

# Summary Stats ###
map.FS <- cbind(map.all, with(map.all, nwea_growth(start.grade= Grade, 
                                                        start.rit = TestRITScore,
                                                        measurementscale = MeasurementScale,
                                                        R42, 
                                                        R22)
                              )
)

map.FS <- mutate(map.FS, 
                 TestQuartile=kipp_quartile(TestPercentile),
                 KIPPTieredGrowth=tiered_growth(TestQuartile, 
                               grade = Grade
                              ),
                 )

fs.match<-s2s_match(map.FS, season1 = "Fall", season2 = "Spring", sy=2014, typical.growth = TRUE, college.ready = TRUE)
ss.match<-s2s_match(map.FS, season1 = "Spring", season2 = "Spring", sy=2014, typical.growth = TRUE, college.ready = TRUE)
ss<-select(ss.match, StudentID, MeasurementScale, S2STarget=TypicalTarget)

fs.plat<-left_join(fs.match, ss, by=c("StudentID", "MeasurementScale"))

fs.plat.3<-filter(fs.plat, Grade==3) %>% mutate(PlatTarget=ifelse(S2STarget<=CollegeReadyTarget|is.na(S2STarget), CollegeReadyTarget, S2STarget),
                                     MetPlat=TestRITScore.2>=PlatTarget)

group_by(fs.plat.3, Grade, MeasurementScale) %>% dplyr::summarise(N=n(),
                                                           N_Meet_Plat=sum(MetPlat, na.rm=T),
                                                           Pct_Meet_Plat=N_Meet_Plat/N)

goldsilver.sum.grade<-filter(fs.match, Grade %in% c(2:3)) %>% 
  group_by(MeasurementScale, Grade) %>% 
  dplyr::summarise(N=n(),
                   N_Meet_Silver=sum(MetTypical, na.rm=T),
                   Pct_Meet_Silver=N_Meet_Silver/N,
                   N_Meet_Gold=sum(MetCollegeReady, na.rm=T),
                   Pct_Meet_Gold=N_Meet_Gold/N)
dplyr::summarise(goldsilver.sum.grade, 
                 N=sum(N), 
                 N_Meet_Silver = sum(N_Meet_Silver), 
                 Pct=N_Meet_Silver/N,
                 N_Meet_Gold = sum(N_Meet_Gold), 
                 Pct=N_Meet_Gold/N)
