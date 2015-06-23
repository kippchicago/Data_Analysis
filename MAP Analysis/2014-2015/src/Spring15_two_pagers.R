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
                                                                   "Mathematics")
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

# test ####
map_mv[['roster']] %>% glimpse

ids <- map_mv$roster %>%
  filter(school=="KCCP",
         grade==5,
         termname == "Spring 2014-2015") %>%
  select(studentid) %>% arrange(studentid) %>% unique

ids <-as.numeric(ids$studentid %>% unique)



# filter mv_object ####

map_mv_filtered <- mapvizieR::mv_filter(
  map_mv,
  roster_filter = quote(map_year_academic == 2014 )
)

x <- knj_two_pager(mapvizieR_obj = map_mv_filtered, studentids =  ids, measurementscale =  "Reading", candidate_start_fws = "Spring", prefer_fws="Spring", start_academic_year = 2013, end_fws = "Spring", end_academic_year = 2014, detail_academic_year = 22200)

# Two pager with report dispatcher ####
cut_list <- list("map_year_academic", 'school', 'grade', 'classname')
call_list <- list(FALSE, FALSE, TRUE, TRUE)

kcs_two_pager_reading <- report_dispatcher(mapvizieR_obj = map_mv_filtered,
                                   cut_list = cut_list,
                                   call_list = call_list,
                                   func_to_call = "knj_two_pager",
                                   arg_list = list(
                                     'measurementscale' = 'Reading',
                                     'candidate_start_fws' = c('Spring', 'Fall'),
                                     'start_year_offsets' = c(-1, 0),
                                     'prefer_fws' = 'Spring',
                                     'end_fws' = 'Spring',
                                     'end_academic_year' = 2014,
                                     'detail_academic_year' = 2099,
                                     'title_text' = quote(paste0(measurementscale, '|', school, '|',
                                                                 grade, '|', classname))
                                     )
                                   )



es <- list()
for (i in c('Reading', 'Mathematics')) {
  es[[i]] <- report_dispatcher(
    mapvizieR_obj = map_mv_filtered,
    cut_list = cut_list,
    call_list = call_list,
    func_to_call = "knj_two_pager",
    arg_list = list(
      'measurementscale' = 'Reading',
      'candidate_start_fws' = c('Spring', 'Fall'),
      'start_year_offsets' = c(-1, 0),
      'prefer_fws' = 'Spring',
      'end_fws' = 'Spring',
      'end_academic_year' = 2014,
      'detail_academic_year' = 2099,
      'title_text' = quote(paste0(measurementscale, ' | ', school, ' | ',
                                  grade, ' | ', classname))
      ),
    verbose = TRUE
  )
}





library(Cairo)
library(stringr)
expander <- 0.3

sch <- mapvizieR::mv_filter(map_mv_filtered,
                            roster_filter = quote(map_year_academic == 2014 )
)[['roster']]$school %>% unique()

#sch<-"KACP"

for (i in c('Reading', 'Mathematics')) {
  for (j in sch) {
    mask <- stringr::str_detect(names(es[[i]]), fixed(j))

    Cairo(
      width = 11 + (expander * 11), height = 8.5 + (expander * 8.5),
      file = paste0("graphs/", j, '_', i, '_two_pagers_by_HR (2013-14 KIPP Comparisons).pdf'),
      type = "pdf", bg = "transparent",
      canvas = "white", units = "in"
    )

    print(es[[i]][mask])

    dev.off()
  }
}


#Just Grade 3
for (i in c('Reading', 'Mathematics')) {
  for (j in sch) {
    mask <- as.logical(stringr::str_detect(names(es[[i]]), fixed(j)) * stringr::str_detect(names(es[[i]]), fixed("grade: 3")))

    Cairo(
      width = 11 + (expander * 11), height = 8.5 + (expander * 8.5),
      file = paste0("graphs/", j, '_', i, '_grade_3.pdf'),
      type = "pdf", bg = "transparent",
      canvas = "white", units = "in"
    )

    print(es[[i]][mask])

    dev.off()
  }
}
