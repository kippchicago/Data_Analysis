require(RJDBC)
require(mapvizieR)
require(Cairo)

source('data//map_all_silo.R')

hrs_qry <- "SELECT * FROM NWEA..ClassAssignments"

hrs<-dbGetQuery(conn, hrs_qry)

map_2 <- map_all_silo %>% left_join(hrs %>% filter(TermName=="Spring 2014-2015") %>%
                                      select(StudentID, current_hr=ClassName) %>%
                                      mutate(StudentID = as.integer(StudentID)),
                                    by=c("StudentID"))

data(ex_CombinedStudentsBySchool)
data(ex_CombinedAssessmentResults)

sel_names<-names(ex_CombinedStudentsBySchool)

sel_cdf_names <- names(ex_CombinedAssessmentResults)
# Drop DisctrictName
#sel_names <- sel_names[sel_names!="DistrictName"]

map_2 <- map_2 %>%
  mutate(Grade=as.integer(ifelse(Grade=="K", "0", Grade)))



map_all_silo_equate <- rbind_list(map_2, map_fall_equate) %>%
  mutate(TestID=ifelse(is.na(TestID), paste(StudentID, TermName, MeasurementScale), TestID))

map_silo <- map_2 %>% filter(MeasurementScale %in% c("Reading",
                                                                   "Mathematics"),
                                           !is.na(TestID),
                                           GrowthMeasureYN=="TRUE"
) %>% mutate(StudentID=as.character(StudentID),
             GrowthMeasureYN=as.logical(GrowthMeasureYN),
             TestDurationMinutes=as.integer(TestDurationMinutes),
             TestRITScore = as.integer(TestRITScore),
             TestPercentile = as.integer(TestPercentile),
             PercentCorrect = as.integer(PercentCorrect)
) %>%
  mutate(DistrictName = "KIPP Chicago")

map_students <- map_silo[, c(sel_names, "MeasurementScale", "TestPercentile", "current_hr")] %>%
  mutate(school=abbrev(SchoolName, exceptions = list(old=c("KAPS", "KAMS"),
                                                     new=c("KACP", "KACP")
  )
  ),
  TestQuartile=kipp_quartile(x=TestPercentile)
  ) %>%
  unique %>%
  as.data.frame

current_students<-map_students %>%
  dplyr::filter(TermName=="Spring 2014-2015") %>%
  dplyr::select(studentid=StudentID, school, Grade) %>%
  unique


# need to fix all the Goal Scores to numeric
# first write a function that does the work
cast_cols_to <- function(.data, col_expression, type_function=as.numeric){
  df<-.data

  cols <- names(df)[grepl(col_expression, names(df))]

  lapply(cols, function(x) df[[x]] <<- type_function(df[[x]]))

  #out

  df


}

map_silo <- cast_cols_to(map_silo, col_expression = "Goal[1-9]RitScore", type_function = as.integer)




map_cdf <- map_silo %>%
  select(-StudentLastName:-Grade) %>%
  as.data.frame

map_mv<-mapvizieR(cdf = map_cdf,
                  roster = map_students,
                  include_unsanctioned_windows=FALSE
)

map_mv_filtered <- mv_filter(map_mv, roster_filter = quote(map_year_academic == 2014 & grade == 0))

# Report Dispatcher time ####
cut_list = list('districtname', 'schoolname', 'grade', 'current_hr')
call_list = list(TRUE, FALSE, TRUE, TRUE)

es <- list()
for (i in c('Reading', 'Mathematics')) {
  es[[i]] <- report_dispatcher(
    mapvizieR_obj = mapvizieR::mv_filter(
      map_mv,
      roster_filter = quote(map_year_academic == 2014)
    ),
    cut_list = cut_list,
    call_list = call_list,
    func_to_call = "knj_two_pager",
    arg_list = list(
      'measurementscale' = i,
      'end_fws' = 'Spring',
      'end_academic_year' =  2014,
      'detail_academic_year' = 2099,
      'title_text' = quote(
        paste0(measurementscale, ' | Gr.', grade, ' | ', current_hr)
      )
    ),
    verbose = TRUE
  )
}


for (i in c('Reading', 'Mathematics')) {
  for (j in sch) {
    mask <- stringr::str_detect(names(es[[i]]), fixed(j))

    Cairo(
      width = 11 + (expander * 11), height = 8.5 + (expander * 8.5),
      file = paste0('data/',j, ' ', i, ' two pagers by HR (2013-14 KIPP Comparisons).pdf'),
      type = "pdf", bg = "transparent",
      canvas = "white", units = "in"
    )

    print(es[[i]][mask])

    dev.off()
  }
}
library(Cairo)

expander <- 0.3

sch <- mapvizieR::mv_filter(map_mv,
                            roster_filter = quote(map_year_academic == 2014)
)[['roster']]$schoolname %>% unique()

