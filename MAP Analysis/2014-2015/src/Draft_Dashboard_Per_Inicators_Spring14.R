# Performance metrics


# get and munge data ####
require(mapvizieR)
require(dplyr)
require(RJDBC)
#  get MAP data ####

setwd("~/Dropbox (KIPP Chicago Schools)/Data Analysis/MAP Analysis/2014-2015/")

silo<-as.data.frame(read.dcf('config//silo_dw.dcf'))


drvr <- JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver',
             '~/Dropbox (KIPP Chicago Schools)/JDBC Drivers/sqljdbc_4.0/enu//sqljdbc4.jar')

conn <- dbConnect(drvr,
                  databaseName=as.character(silo$dbname),
                  url=as.character(silo$url),
                  user=as.character(silo$user),
                  password=as.character(silo$password)
)

qry<-"SELECT * FROM NWEA..MAP$comprehensive#cps_included WHERE GrowthMeasureYN='TRUE'"

map_all_silo<-dbGetQuery(conn, qry)

qry<-"SELECT * FROM NWEA..ClassAssignments"

map_hrs_silo<-dbGetQuery(conn, qry)

#names(map_hrs_silo) <- tolower(names(map_hrs_silo))

data(ex_CombinedStudentsBySchool)
data(ex_CombinedAssessmentResults)

sel_names<-names(ex_CombinedStudentsBySchool)

sel_cdf_names <- names(ex_CombinedAssessmentResults)
# Drop DisctrictName
sel_names <- sel_names[sel_names!="DistrictName"]

map_all_silo <- map_all_silo %>%
  mutate(Grade=as.integer(ifelse(Grade=="K", "0", Grade)))

# need to recast goal scores from character to numeric

cast_goal_to_numeric <- function(df) {
  target_columns <- names(df)[grepl("Goal[1-9]RitScore", names(df))]

  out<-lapply(target_columns, function(x) df[[x]]<<-as.numeric(df[[x]]))

  df

}

map_all_silo_2 <- cast_goal_to_numeric(map_all_silo)

map_silo <- map_all_silo_2 %>% filter(MeasurementScale %in% c("Reading",
                                                              "Mathematics",
                                                              "Science - General Science")
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
  left_join(map_hrs_silo, by=c("TermName", "StudentID")) %>%
  #unique %>%
  rename(SchoolName=SchoolName.x) %>%
  select(-SchoolName.y) %>%
  as.data.frame

current_students<-map_students %>%
  dplyr::filter(TermName=="Spring 2014-2015") %>%
  dplyr::select(studentid=StudentID, school, Grade) %>%
  unique



map_cdf <- map_silo %>%
  select(-StudentLastName:-Grade) %>%
  as.data.frame



map_mv<-mapvizieR(cdf = map_cdf,
                  roster = map_students,
                  include_unsanctioned_windows=FALSE
)

# calc_me_perf function ####

calc_me_perf <- function(mv_obj,
                         year = 2014,
                         measurementscale = c("Reading", "Mathematics", "General Science"),
                         fall_start_grades = NULL,
                         drop_grades = NULL,
                         group_by_vars
                         ) {


  mv_obj$growth_df <- mv_obj$growth_df %>%
    dplyr::mutate(end_schoolname = ifelse(grepl("Ascend", end_schoolname),
                                   "KIPP Ascend", end_schoolname))

  grades <- c(0:8)

  if(!missing(drop_grades)){
    grades <- dplyr::setdiff(grades, drop_grades)
  }

  # calcualte f2s and s2s sets and pull growth data for each, recombine
  if (!missing(fall_start_grades)) {
    spring_start_grades <- setdiff(grades, fall_start_grades)

    f2s <- mv_obj$growth_df %>%
      dplyr::filter(end_map_year_academic == year,
                    measurementscale %in% measurementscale,
                    end_fallwinterspring == "Spring",
                    start_fallwinterspring == "Fall",
                    end_grade %in% fall_start_grades
                    )

    s2s <- mv_obj$growth_df %>%
      dplyr::filter(end_map_year_academic == year,
                    measurementscale %in% measurementscale,
                    end_fallwinterspring == "Spring",
                    start_fallwinterspring == "Spring",
                    end_grade %in% spring_start_grades
                    )

    growth_data <- dplyr::rbind_list(f2s, s2s)
  } else {
    growth_data <- mv_obj$growth_df %>%
      dplyr::filter(end_map_year_academic == year,
                    measurementscale %in% measurementscale,
                    end_fallwinterspring == "Spring",
                    start_fallwinterspring == "Spring",
                    end_grade %in% grades
      )
  }

  #summarize growth data
  out <- growth_data %>%
    dplyr::group_by_(.dots = group_by_vars) %>%
    dplyr::summarize(N = n(),
                     N_typical = sum(met_typical_growth, na.rm = TRUE),
                     Pct_typical = N_typical/N)

  # return
  out
}





# KIPP RC (FS 0,1,2 & 5) ####

calc_me_perf(map_mv,
             year = 2014,
             fall_start_grades=c(0,1,2,5),
             group_by_vars=c("measurementscale")
             )


# KIPP RC (FS 0,2 & 5) ####

calc_me_perf(map_mv,
             year = 2014,
             fall_start_grades=c(0,2,5),
             group_by_vars=c("measurementscale")
)

# Scenario S2S  -5 : Spring-to-Spring 3, 4, 6, 7, 8 ####

calc_me_perf(map_mv,
             year = 2014,
             drop_grades = c(0,1,2,5),
             group_by_vars = c("measurementscale")
)

# * Scenario SQRP: Spring-to-Spring 3, 4, 5*, 6, 7, 8  ####



# % of 8th graders above 50th percentile
map_mv$cdf %>%
  filter(map_year_academic == 2014, fallwinterspring=="Spring", grade == 8) %>%
  group_by(measurementscale) %>%
  summarize(N = n(),
            ge50 = sum(testpercentile>=50),
            pct50 = ge50/N)


