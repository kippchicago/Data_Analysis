# Team helper functions

localizations <- list(
  'default'=list(
    #cuts the ribbons in ACT rainbow/height weight plots
    'act_cuts' = c(11, 16, 18, 22, 25, 29)
    #indicates what act lines to highlight on plots
    ,'act_trace_lines' = c(11, 16, 18, 22, 25, 29)
    ,'canonical_colleges' = c(
      '2 year/Community'
      ,'Local Public'
      ,'Regional Public'
      ,'Flagship State'
      ,'Flagship State (Honors Program)'
      ,'Top 40'
    )
    ,'grad_rates' = c(rep('', 6))
    
  )
  ,'Newark'=list(
    'act_cuts' = c(8, 16, 18, 21, 23, 26, 29)  
    ,'act_trace_lines' = c(8, 16, 18, 21, 23, 26, 29, 32)
    ,'canonical_colleges' = c(
      'Essex County'
      ,'Bloomfield College'
      ,'Caldwell College'
      ,'Drew University'
      ,'Rutgers University'
      ,'Rutgers (Honors Program)'
      ,'Cornell'
    )
    ,'grad_rates' = c(
      '5%'
      ,'29%'
      ,'52%'
      ,'71%'
      ,'77%'
      ,'85%'
      ,'92%'
    )
  )
  ,'Chicago'=list(
    'act_cuts' = c(11, 16, 18, 22, 25, 29)  
    ,'act_trace_lines' = c(11, 16, 18, 22, 25, 29)  
    ,'canonical_colleges' = c(
      'Harold Washington'
      ,'Chicago State University'
      ,'UIC'
      ,'University of Illinois'
      ,'Illinois (Honors Program)'
      ,'University of Chicago'
    )
    ,'grad_rates' = c(
      '99%'
      ,'99%'
      ,'99%'
      ,'99%'
      ,'99%'
      ,'99%'
    )
  )
)

localize <- function(region) {
  final_list <- list()
  #number of items
  loc_var <- names(localizations[['default']])
  #how many
  loc_var_count <- length(loc_var)
  
  counter <- 0
  #iterate over defaults
  for (i in loc_var) {
    #look up the item
    custom <- localizations[[region]][i]
    #if it's null, return the default.  otherwise, the custom value.
    if (is.null(custom)) {
      final_list[i] <- localizations[['default']][i]
      counter <- counter + 1
    } else {
      final_list[i] <- custom
    }
  }
  if (counter == loc_var_count) {
    cat('Your localization choice did not match any known options!
        To store a localization, add a list to localizations.
        For example:
        localizations[[\'Ridgemont High\']] <- list(
        \'act_cuts\' = c(12, 17, 19, 22, 24, 27, 32)
    )
        '
    )
  }
  
  if (loc_var_count - counter >= loc_var_count) {
    writeLines(sprintf('Localized %s variables', loc_var_count - counter))
  }
  
  return(final_list)
  }

get_demographics <- function() {
  require(RODBC)
  
  #connect to db
  db <- RODBC::odbcConnect(dsn='NARDO')
  
  query <- "
  WITH enr AS
  (SELECT cc.termid AS termid
  ,cc.studentid
  ,SUBSTRING(s.first_name, 1, 1) + '. ' + s.last_name AS stu_name
  ,sections.id AS sectionid
  ,sections.section_number
  ,teachers.first_name + '_' + teachers.last_name AS teacher
  ,sections.course_number + '|' + sections.section_number AS homeroom_simple
  ,sections.course_number + '|' + sections.section_number + ' - ' + teachers.last_name AS homeroom
  ,sections.section_number + '_' + teachers.first_name + '_' + teachers.last_name AS enr_hash
  ,courses.course_number
  ,courses.course_name
  ,s.grade_level
  ,sch.abbreviation AS school
  FROM KIPP_NJ..CC
  JOIN KIPP_NJ..SECTIONS
  ON cc.sectionid = sections.id
  AND cc.termid >= 2300
  JOIN KIPP_NJ..TEACHERS
  ON sections.teacher = teachers.id
  JOIN KIPP_NJ..COURSES
  ON sections.course_number = courses.course_number
  AND courses.course_number LIKE '%HR%'
  JOIN KIPP_NJ..STUDENTS s
  ON cc.studentid = s.id
  AND s.enroll_status = 0
  JOIN KIPP_NJ..SCHOOLS sch
  ON s.schoolid = sch.school_number
  WHERE cc.dateenrolled <= GETDATE()
  AND cc.dateleft >= GETDATE()
  )
  SELECT c.studentid
  ,s.student_number
  ,s.lastfirst
  ,s.first_name
  ,s.first_name + ' ' + s.last_name AS stu_name
  ,c.grade_level
  ,sch.abbreviation AS school
  ,ISNULL(enr.homeroom, 'NONE') AS homeroom
  FROM KIPP_NJ..COHORT$comprehensive_long c
  JOIN KIPP_NJ..STUDENTS s
  ON c.studentid = s.id
  AND s.enroll_status = 0
  AND c.year = 2013
  AND c.rn = 1
  JOIN KIPP_NJ..SCHOOLS sch
  ON c.schoolid = sch.school_number
  LEFT OUTER JOIN enr
  ON c.studentid = enr.studentid
  ORDER BY sch.abbreviation
  ,c.grade_level
  ,enr.homeroom
  ,s.lastfirst"
  
  query <- strwrap(query, width=100000, simplify=TRUE)
  
  #execute and return
  data <- sqlQuery(db, query, stringsAsFactors = FALSE)  
  
  #close the connection to db
  odbcCloseAll()
  
  #return the data
  return(data)
}

capture_run <- function(
  files
  ,params_df
) {
  #build a hash
  unq_name <- as.character(format(Sys.time(), "%Y %m %d %H_%M_%S"))
  unq_name <- gsub(':', '', unq_name)
  unq_name <- gsub(' ', '_', unq_name)
  
  run_name <- paste('RR_stu_rept_', unq_name, sep = '')
  dir_name <- paste('//team-kaseya/faculty/amartin/r runs/', run_name, sep = '')
  
  #make directory
  dir.create(dir_name)
  
  #copy the files
  for(f in files) {
    file.copy(
      from = f
      ,to = dir_name
    )    
  }
  
  #save run params
  write.csv(
    x = params_df
    ,file = paste(dir_name, '/run_params.csv', sep = '')
  )
}

get_cur_MAP <- function() {
  require(RODBC)
  
  #connect to db
  db <- RODBC::odbcConnect(dsn='NARDO')
  
  query <- "
  SELECT sub.*
  FROM
  (SELECT map.ps_studentid
  ,map.measurementscale
  ,map.testritscore
  ,map.proj_ACT_subj_score
  ,map.map_year_academic
  ,map.termname
  ,ROW_NUMBER() OVER
  (PARTITION BY map.ps_studentid
  ,map.measurementscale
  ORDER BY map.map_year DESC
  ,map.fallwinterspring_numeric DESC
  ) AS rn_last
  FROM KIPP_NJ..MAP$comprehensive#identifiers map
  JOIN KIPP_NJ..STUDENTS s
  ON map.ps_studentid = s.id
  AND s.enroll_status = 0
  ) sub
  WHERE rn_last = 1
  AND map_year_academic >= 2012"
  
  query <- strwrap(query, width=100000, simplify=TRUE)
  
  #execute and return
  data <- sqlQuery(db, query, stringsAsFactors = FALSE)  
  
  #close the connection to db
  odbcCloseAll()
  
  #return the data
  return(data)
}

get_DIY <- function() {
  require(RODBC)
  
  #connect to db
  db <- RODBC::odbcConnect(dsn='NARDO')
  
  query <- "
  SELECT sub.*
  FROM
  (SELECT map.ps_studentid
  ,map.measurementscale
  ,map.testritscore
  ,map.proj_ACT_subj_score
  ,map.map_year_academic
  ,map.termname
  ,ROW_NUMBER() OVER
  (PARTITION BY map.ps_studentid
  ,map.measurementscale
  ORDER BY map.map_year DESC
  ,map.fallwinterspring_numeric DESC
  ) AS rn_last
  FROM KIPP_NJ..MAP$comprehensive#identifiers map
  JOIN KIPP_NJ..STUDENTS s
  ON map.ps_studentid = s.id
  AND s.enroll_status = 0
  ) sub
  WHERE rn_last = 1
  AND map_year_academic >= 2012"
  
  query <- strwrap(query, width=100000, simplify=TRUE)
  
  #execute and return
  data <- sqlQuery(db, query, stringsAsFactors = FALSE)  
  
  #close the connection to db
  odbcCloseAll()
  
  #return the data
  return(data)
}

adhoc_Nardo <- function(query) {
  require(RODBC)
  
  #connect to db
  db <- RODBC::odbcConnect(dsn='NARDO')
  
  
  query <- strwrap(query, width=100000, simplify=TRUE)
  
  #execute and return
  data <- sqlQuery(db, query, stringsAsFactors = FALSE)  
  
  #close the connection to db
  odbcCloseAll()
  
  #return the data
  return(data)
}

minimal_table <- function(data) {
  require(xtable)
  
  #convert to DF
  df <- data.frame(data, stringsAsFactors = FALSE)
  
  #print via xtable, capture HTML
  t <- print(
    xtable(df)
    ,type="html"
    ,print.results = FALSE
    ,include.rownames = FALSE
    ,include.colnames = FALSE
  )
  
  #new CSS
  t <- gsub('<TABLE border=1>', '<table class="gridtable">', t)
  
  return(t)
}

get_MAP_history <- function() {
  query <- "WITH cohort_decode AS
  (SELECT c.studentid AS studentid
  ,c.grade_level AS hist_grade_level
  ,c.year
  ,ROW_NUMBER() OVER
  (PARTITION BY c.studentid
  ,c.grade_level
  ORDER BY year DESC
  ) AS holdover_decode_rn
  FROM KIPP_NJ..COHORT$comprehensive_long#static c
  WHERE c.rn = 1
  )
  ,map_format AS
  (SELECT m.ps_studentid AS studentid
  ,m.measurementscale
  ,m.map_year_academic
  ,m.fallwinterspring
  ,CASE
  WHEN m.fallwinterspring = 'Spring' THEN m.grade_level
  WHEN m.fallwinterspring = 'Winter' THEN m.grade_level - 0.5
  WHEN m.fallwinterspring = 'Fall'   THEN m.grade_level - 0.8
  END AS grade_level_x
  ,CASE
  WHEN m.fallwinterspring = 'Fall' 
  THEN CAST(m.grade_level AS VARCHAR) + ' Fall: ' + CAST(m.testritscore AS VARCHAR)
  WHEN m.fallwinterspring = 'Winter' 
  THEN CAST(m.grade_level AS VARCHAR) + ' Win: ' + CAST(m.testritscore AS VARCHAR)
  WHEN m.fallwinterspring = 'Spring' 
  THEN CAST(m.grade_level AS VARCHAR) + ' Spr: ' + CAST(m.testritscore AS VARCHAR)
  END AS test_label
  ,m.testritscore
  FROM KIPP_NJ..MAP$comprehensive#identifiers m
  JOIN KIPP_NJ..STUDENTS s
  ON m.ps_studentid = s.id
  AND s.enroll_status = 0
  AND s.schoolid IN (73252, 133570965)
  WHERE m.measurementscale IN ('Reading', 'Mathematics')
  AND m.rn = 1
  )
  SELECT c.studentid
  ,c.hist_grade_level
  ,s.lastfirst
  ,s.first_name + ' ' + s.last_name AS stu_name
  ,s.grade_level AS cur_grade_level
  ,sch.abbreviation AS cur_school
  ,map_format.*
  FROM cohort_decode c
  JOIN KIPP_NJ..STUDENTS s
  ON c.studentid = s.id
  JOIN KIPP_NJ..SCHOOLS sch
  ON s.schoolid = sch.school_number
  AND s.enroll_status = 0
  JOIN map_format
  ON c.studentid = map_format.studentid
  AND c.year = map_format.map_year_academic
  WHERE c.holdover_decode_rn = 1"
  
  d <- adhoc_Nardo(query)
  
  return(d)
}


oracle_pull <- function(query) {
  
  secret = readChar('C:/Users/AMartin/Dropbox/TEAM 2012-2013/Analysis/Logistical/secret.txt',nchars=1e6)
  XE = odbcConnect('KIPP_NWK', uid='KIPP_NWK', pwd=secret, believeNRows=FALSE)
  
  d <- sqlQuery(XE, query)
  
  #close the connection to db
  odbcCloseAll()
  
  return(d)
}

rainbow_colors <- function() {
  require(grDevices)
  
  rainbow_all <- rainbow(20)
  rainbow_subset <- c(rainbow_all[2:6], rainbow_all[9:17])
  my_colors <- rainbow_subset   
  
  return(my_colors)
}


rit_height_weight_npr <- function(
  desired_subj
  ,color_list = rainbow_colors()
  ,ribbon_alpha = .35
  ,annotation_style = 'points'
  ,line_style = 'none'
) {
  require(ggplot2)
  e <- new.env()
  
  npr_query <- ("
                SELECT measurementscale
                ,CASE 
                WHEN fallwinterspring = 'Fall' THEN grade - 0.8
                ELSE grade
                END AS grade
                ,rit
                ,percentile
                FROM map$norms_2011_dense_by_npr
                WHERE (fallwinterspring = 'Spring' OR (grade = 0 AND fallwinterspring = 'Fall'))
                AND (percentile IN (1,5,95,99) OR
                mod(percentile, 10) = 0)
                AND measurementscale = 'REPLACEME'
                ORDER BY measurementscale
                ,grade
                ,rit")
  
  npr_query <- sub('REPLACEME', desired_subj, npr_query)
  npr_query <- strwrap(npr_query, width=100000, simplify=TRUE)
  
  #get data from XE
  norms_dense <- oracle_pull(npr_query)
  
  #add y axis margins
  placeholder1 <- norms_dense[norms_dense$GRADE == 11,]
  #arbitrary, just needs to be bigger than 11
  placeholder1$GRADE <- 14
  norms_dense <- rbind(norms_dense, placeholder1)
  
  placeholder2 <- norms_dense[norms_dense$GRADE == -0.8,]
  #arbitrary, just needs to be smaller than -0.8
  placeholder2$GRADE <- -3
  norms_dense <- rbind(norms_dense, placeholder2)
  e$norms_dense <- norms_dense[with(norms_dense, order(MEASUREMENTSCALE, GRADE)), ]
  
  #had a lot of trouble here. 
  #cutting into ribbon bins
  e$npr_grades <- c(-3,-0.8,0,1,2,3,4,5,6,7,8,9,10,11,14)
  e$nprs <- c(1,5,10,20,30,40,50,60,70,80,90,95,99)
  
  e$npr_band01 <-  subset(e$norms_dense, PERCENTILE == e$nprs[1])
  e$npr_band05 <-  subset(e$norms_dense, PERCENTILE == e$nprs[2])
  e$npr_band10 <- subset(e$norms_dense, PERCENTILE == e$nprs[3])
  e$npr_band20 <- subset(e$norms_dense, PERCENTILE == e$nprs[4])
  e$npr_band30 <- subset(e$norms_dense, PERCENTILE == e$nprs[5])
  e$npr_band40 <- subset(e$norms_dense, PERCENTILE == e$nprs[6])
  e$npr_band50 <- subset(e$norms_dense, PERCENTILE == e$nprs[7])
  e$npr_band60 <- subset(e$norms_dense, PERCENTILE == e$nprs[8])
  e$npr_band70 <- subset(e$norms_dense, PERCENTILE == e$nprs[9])
  e$npr_band80 <- subset(e$norms_dense, PERCENTILE == e$nprs[10])
  e$npr_band90 <- subset(e$norms_dense, PERCENTILE == e$nprs[11])
  e$npr_band95 <- subset(e$norms_dense, PERCENTILE == e$nprs[12])
  e$npr_band99 <- subset(e$norms_dense, PERCENTILE == e$nprs[13])
  
  #what is needed is a data frame with ribbon, x, ymin, and ymax
  #make them per band, then rbind  
  #first make the top and bottom - custom
  e$df_npr1 <- data.frame(
    rib = rep('below_1', 15)
    ,x = e$npr_band01$GRADE
    #dummy value - just needs to be small
    ,ymin = rep(100, 15)
    ,ymax = e$npr_band01$RIT
  )
  e$df_npr99 <- data.frame(
    rib = rep('above_99', 15)
    ,x = e$npr_band99$GRADE
    #dummy value - just needs to be big
    ,ymin = e$npr_band99$RIT
    ,ymax = rep(300, 15)
  )
  e$df <- rbind(e$df_npr1, e$df_npr99)
  
  #then generate the others in a loop
  bands <- ls(pattern="npr_band*", envir=e)
  
  #list to hold ribbon names
  e$ribbons <- rep(NA, 12)
  
  for (i in 1:(length(bands)-1)) {
    new_df_name <- paste(bands[i], bands[i+1], sep='_')
    #remove 'band'
    new_df_name <- gsub('band', '', new_df_name)
    
    #lower and upper df
    lower <- get(bands[i], envir=e)
    upper <- get(bands[i+1], envir=e)
    
    #make a new df for this ribbon
    inner_df <- data.frame(
      rib = rep(new_df_name, 15)
      ,x = e$npr_grades
      ,ymin = lower$RIT
      ,ymax = upper$RIT
    )
    
    #rbind to existing df
    e$df <- rbind(e$df, inner_df)
    #update list of ribbons
    e$ribbons[i] <- new_df_name
  }
  
  #now make the geom_ribbons
  #first make top & bottom
  e$rib_under_1 <- geom_ribbon(
    data = e$df[e$df$rib == 'below_1', ]
    ,aes(
      x = x
      ,ymin = ymin
      ,ymax = ymax
    )
    ,fill = color_list[1]
    ,alpha = ribbon_alpha
    ,environment = e
  )
  e$rib_above_99 <- geom_ribbon(
    data = e$df[e$df$rib == 'above_99', ]
    ,aes(
      x = x
      ,ymin = ymin
      ,ymax = ymax
    )
    ,fill = color_list[14]
    ,alpha = ribbon_alpha
    ,environment = e
  )
  
  for (i in 1:length(e$ribbons)) {
    new_rib_name <- paste('rib', e$ribbons[i], sep='_')
    #make ribbon
    inner_ribbon <- geom_ribbon(
      data = e$df[e$df$rib == e$ribbons[i], ]
      ,aes(
        x = x
        ,ymin = ymin
        ,ymax = ymax
      )
      ,fill = color_list[i+1]
      ,alpha = ribbon_alpha
      ,environment = e       
    )
    
    #appropriate df
    assign(new_rib_name, inner_ribbon, envir=e)
  }
  
  #base ggplot 
  p <- ggplot(
    data = norms_dense
    ,environment = e
  )
  
  #annotation style options
  if (grepl('points', annotation_style)) {
    npr_annotation <- geom_point(
      aes(
        x = GRADE
        ,y = RIT
      )
    )
  } else if (grepl('big numbers', annotation_style)) {
    npr_annotation <- geom_text(
      aes(
        x = GRADE
        ,y = RIT
        ,label = PERCENTILE
      )
    )
  } else if (grepl('small numbers', annotation_style)) {
    npr_annotation <- geom_text(
      aes(
        x = GRADE
        ,y = RIT
        ,label = PERCENTILE
      )
      ,size = 3  
      ,fontface="italic"
      ,color = 'gray40'
      ,alpha = 0.8
    ) 
  } else {
    npr_annotation <- NULL
  } 
  
  #lines
  if (grepl('gray lines', line_style)) {
    npr_lines <- geom_line(
      aes(
        x = GRADE
        ,y = RIT
        ,group = PERCENTILE        
      )
      ,size = 0.5
      ,color = 'gray80'
    )
  } else {
    npr_lines <- NULL
  }
  
  #put it all together
  p <- p + 
    e$rib_under_1 + 
    e$rib_npr_01_npr_05 +
    e$rib_npr_05_npr_10 +
    e$rib_npr_10_npr_20 +
    e$rib_npr_20_npr_30 +
    e$rib_npr_30_npr_40 +
    e$rib_npr_40_npr_50 +
    e$rib_npr_50_npr_60 +
    e$rib_npr_60_npr_70 +
    e$rib_npr_70_npr_80 +
    e$rib_npr_80_npr_90 +
    e$rib_npr_90_npr_95 +
    e$rib_npr_95_npr_99 +
    e$rib_above_99 +   
    npr_annotation +
    npr_lines
  
  return(p)
}


base_rit_hist_plot <- function(
  subj
) {
  #colors
  grayscale <- c(rep(c('white','gray70'),6),'white','white')
  
  p <- rit_height_weight_npr(
    desired_subj = subj
    ,color_list = grayscale
    ,annotation_style = 'small numbers'
    ,line_style = 'gray lines'
  ) + 
    theme_bw() +
    theme(
      axis.title.x = element_blank()
      ,axis.title.y = element_blank()
    )
  
  return(p)
}



generate_ACT_RIT_df <- function() {
  #empty vectors to hold data
  act=c()
  grade=c()
  subject=c()
  rit=c()
  school=c()
  cohort=c()
  act_model=data.frame()
  
  #vectors to loop over
  act_levels=c(c(1:21),21.1,c(22:36))
  grades=c(-1,-0.7,c(0:11))
  
  #for loop that plugs in various ACT and grade levels to model
  for(i in act_levels) {
    for(j in grades) {   
      #new model (post call with DC, thanks Max)
      rit <- c(
        rit
        ,round(
          (-4.26*j + .183*(j^2) - i - 30.543) / -.3071 
          ,1
        )
      )
      act <- c(act, i)
      grade <- c(grade, j)
      subject <- c(subject, "Math")
      school <- c(school, "Model")
      cohort <- c(cohort, 0)
    }
  }
  
  #for loop that plugs in various ACT and grade levels to model
  for(i in act_levels) {
    for(j in grades) {
      #new model (post call with DC, thanks Max)
      rit <- c(
        rit
        ,round(
          (-3.433*j + .132*(j^2) - i - 54.403) / -.417
          ,1
        )
      )
      act <- c(act, i)
      grade <- c(grade, j)
      subject <- c(subject, "Reading")
      school <- c(school, "Model")
      cohort <- c(cohort, 0)
    }
  }
  
  act_model <- data.frame(
    school=school,
    cohort=cohort, 
    grade=grade,
    rit=rit,
    act=act,
    subject=subject
  )
  
  return(act_model)
}



rit_height_weight_ACT <- function(
  desired_subj
  ,color_list = rainbow_colors()
  ,annotation_style = 'points'
  ,line_style = 'none'
  ,school_type = 'MS'
  ,localization = localize('Newark')
) {
  require(ggplot2)
  e <- new.env()
  
  #get ACT data frame (fitted model)
  act_df <- generate_ACT_RIT_df()
  #subset  
  act_df <- act_df[act_df$subject==desired_subj & act_df$act != 21.1, ]
  
  chart_settings <- list(
    'MS'=list(
      'y_disp_min' = 180
      ,'y_disp_max' = 290
      ,'ribbon_alpha' = .3
      ,'college_text_color' = (alpha("gray50", 0.4))
      ,'college_name_size' = 4.5
      ,'chart_angle' = 27
      ,'act_lines_alpha' = .3
      ,'act_x' = 10.8
      ,'act_grade_for_y' = 11
      ,'act_color' = (alpha("gray50",0.6))
      ,'act_size' = 3
      ,'act_angle' = 6
      ,'act_vjust' = 0.5
      ,'act_hjust' = 0.65
    )
    ,'ES'=list(
      'y_disp_min' = 130
      ,'y_disp_max' = 220
      ,'ribbon_alpha' = .3
      ,'college_text_color' = (alpha("gray50", 0.4))
      ,'college_name_size' = 4.5
      ,'chart_angle' = 30
      ,'act_lines_alpha' = .3
      ,'act_x' = -0.7
      ,'act_grade_for_y' = -0.7
      ,'act_color' = (alpha("gray50",0.6))
      ,'act_size' = 3
      ,'act_angle' = 15
      ,'act_vjust' = 0.5
      ,'act_hjust' = 0.2   
    )
  )
  
  #what flavor of chart are we making?
  active_settings = chart_settings[[school_type]]
  
  #we have to add some 'margin' above and below the maximum of our plot
  #values are basically arbitrary but needed so that charts don't truncate in a weird way.
  #y axis margin big
  placeholder1 <- act_df[act_df$grade == 11, ]
  #arbitrary, just needs to be bigger than 11
  placeholder1$grade <- 14
  act_df <- rbind(act_df, placeholder1)
  
  #y axis margin small
  placeholder2 <- act_df[act_df$grade == -1, ]
  #arbitrary, just needs to be smaller than -1
  placeholder2$grade <- -3
  act_df <- rbind(act_df, placeholder2)
  
  e$act_df <- act_df[with(act_df, order(grade)), ]
  
  #make ribbons and labels here
  #add 1 and 36 to personalized cuts
  act_bands <- c(min(act_df$act), localization$act_cuts, 36)
  
  #store names of everything that gets made
  ribbon_names = c()
  
  #iterate over the list of bands and make stuff
  for (i in 1:(length(act_bands)-1)) {
    #which cuts
    low_cut <- act_bands[i]
    high_cut <- act_bands[i+1]
    
    #subset the main act df
    lower <- act_df[act_df$act==low_cut, ]
    higher <- act_df[act_df$act==high_cut, ]
    
    #as df
    inner_df <- data.frame(
      x=lower$grade
      ,ymin=lower$rit
      ,ymax=higher$rit
    )
    
    #now make a ribbon
    #first give it a name
    new_rib_name <- paste0('rib', '_', low_cut, '_', high_cut)
    
    #make it
    inner_ribbon <- geom_ribbon(
      data = inner_df
      ,aes(
        x = x
        ,ymin = ymin
        ,ymax = ymax
      )
      ,fill = color_list[i+1]
      ,alpha = active_settings$ribbon_alpha
      ,environment = e      
    )
    
    #assign variable name
    assign(new_rib_name, inner_ribbon, envir=e)
    
    #add to list of ribbons
    ribbon_names = c(ribbon_names, new_rib_name)
  }
  
  #base ggplot 
  p <- ggplot(
    data = act_df[act_df$act %in% localization$act_cuts, ]
    ,environment = e
  ) +
    theme_bw()
  
  #put all the ribbons on it
  for (i in ribbon_names) {
    p <- p + get(i, env=e)
  }
  
  #annotation style options
  if (grepl('points', annotation_style)) {
    act_annotation <- geom_point(
      aes(
        x = grade
        ,y = rit
      )
    )
  } else if (grepl('big numbers', annotation_style)) {
    act_annotation <- geom_text(
      data = act_df[act_df$act %in% localization$act_trace_lines, ]
      ,aes(
        x = grade
        ,y = rit
        ,label = act
      )
    )
  } else if (grepl('small numbers', annotation_style)) {
    act_annotation <- geom_text(
      data=act_df[act_df$act %in% localization$act_trace_lines, ]
      ,aes(
        x = grade
        ,y = rit
        ,label = act
      )
      ,size = 3  
      ,fontface="italic"
      ,color = 'gray40'
      ,alpha = 0.8
    ) 
  } else {
    act_annotation <- NULL
  }
  
  #lines
  if (grepl('gray lines', line_style)) {
    act_lines <- geom_line(
      data = act_df[act_df$act %in% localization$act_trace_lines, ]
      ,aes(
        x = grade
        ,y = rit
        ,group = act        
      )
      ,size = 0.5
      ,color = 'gray80'
    )
  } else {
    act_lines <- NULL
  }
  
  #put it together
  p <- p + 
    act_annotation +
    act_lines 
  
  return(p)
}


stu_RIT_hist_plot_elements <- function(
  stu_map_df
) {
  require(plyr)
  require(scales)
  #calculate min/max x and y
  min_y <- round_any(
    x = min(stu_map_df$testritscore)
    ,accuracy = 10
    ,f = floor
  )
  
  max_y <- round_any(
    x = max(stu_map_df$testritscore)
    ,accuracy = 10
    ,f = ceiling
  )
  
  min_x <- round_any(
    x = min(stu_map_df$grade_level_x)
    ,accuracy = 1
    ,f = floor
  )
  
  max_x <- round_any(
    x = max(stu_map_df$grade_level_x)
    ,accuracy = 1
    ,f = ceiling
  )  
  
  #add a line showing previous scores
  rit_hist_line <- geom_line(
    aes(
      x = grade_level_x
      ,y = testritscore
    )
    ,data = stu_map_df
    ,size = 1.5
  ) 
  
  # show test events
  rit_hist_points <- geom_point(
    aes(
      x = grade_level_x
      ,y = testritscore
    )
    ,data = stu_map_df
    ,shape = 21
    ,color = 'white'
    ,size = 4
    ,fill = 'white'
    ,alpha = 0.9
  )
  
  #label
  rit_hist_text <- geom_text(
    aes(
      x = grade_level_x
      ,y = testritscore
      ,label = test_label
    )
    ,data = stu_map_df
    ,size = 3
    ,color = 'gray30'
    ,vjust = 1.5
  )
  
  return(
    list(
      'plot_elements' = list(
        'line' = rit_hist_line
        ,'points' = rit_hist_points
        ,'text' = rit_hist_text
      )
      ,'plot_limits_round' = list(
        'min_x' = min_x
        ,'max_x' = max_x
        ,'min_y' = min_y
        ,'max_y' = max_y
      )      
      ,'plot_limits_exact' = list(
        'min_x' = min_x - 0.1
        ,'max_x' = max_x + 0.1
        ,'min_y' = min_y - 1
        ,'max_y' = max_y + 1
      )
    )
  )
}


#in the interest of DRY, abstract out writing college labels
#this is non-trivial.
college_label_element <- function(
  xy_lim_list
  ,desired_subj
  ,labels_at_grade = 6
  ,localization = localize('Newark')
  ,aspect_ratio = 1
) {
  require(dplyr)
  require(ggplot2)
  
  act_df <- generate_ACT_RIT_df()
  act_bands <- c(localization$act_cuts, max(act_df$act))
  
  labels_df <- act_df[act_df$subject == desired_subj & 
                        act_df$act %in% act_bands &
                        act_df$grade == labels_at_grade, ]  
  
  valid_ys <- na.omit(labels_df$rit + 0.5 * (lead(labels_df$rit, 1) - labels_df$rit))
  
  #calculus, bitches, to find the angle of the label.
  #angle is the slope of the tangent line to the curve.
  #all the ACT curves have the same form, so we just need to get the slope once.
  
  #slope of tangent line to y=ax^2 + bx +c is 2ap + b where p is the x value in question.
  #(read about it: http://www.math.dartmouth.edu/opencalc2/cole/lecture3.pdf)
  #x value in question = labels_at_grade parameter
  #so... we just need the coefficients for the act slopes
  
  #coefficients for ACT 23 because why not
  coefs <- list(
    'Math'=list(
      'a'=-0.5961, 'b'=13.8734, 'c'='irrelevant (174.347)'
    )
    ,'Reading'=list(
      'a'=-0.3161, 'b'=8.2295, 'c'='irrelevant (185.6195)'
    )  
  )
  
  #get the active coefs
  active_coefs = coefs[[desired_subj]]
  
  #calculate slope of tangent line
  tan_slope = 2 * active_coefs[['a']] * labels_at_grade + 0.5 + active_coefs[['b']]
  
  #need to account for viewport..
  plot_xrange <- xy_lim_list[['max_x']] - xy_lim_list[['min_x']]
  plot_yrange <- xy_lim_list[['max_y']] - xy_lim_list[['min_y']]
  
  asp <- plot_xrange / plot_yrange 
  
  college_text <- geom_text(
    data = data.frame(
      x_pos = rep(labels_at_grade, length(valid_ys))
      ,y_val = valid_ys
      ,label = localization$canonical_colleges
    )
    ,aes(
      x = x_pos
      ,y = y_val
      ,label = label
    )  
    ,angle = 180/pi*atan(tan_slope * asp *aspect_ratio)
    ,hjust = 1
    ,vjust = 0.5
  )
  
  return(college_text)
}


build_student_college_plot <- function(
  base_plot
  ,stu_map_df
  ,desired_subj
  ,labels_at_grade = 6
  ,localization = localize('Newark')
  ,aspect_ratio = 1
) {
  
  #1. get stu_RIT_hist_plot_elements
  stu_elements <- stu_RIT_hist_plot_elements(stu_map_df)
  
  #2. with data from step 1, put the college labels at the right spot
  college_labels <- college_label_element(
    xy_lim_list=stu_elements[['plot_limits_exact']]
    ,desired_subj=desired_subj
    ,labels_at_grade = labels_at_grade
    ,localization = localization,
    aspect_ratio=aspect_ratio
  ) 
  
  min_y <- stu_elements[['plot_limits_round']][['min_y']]
  max_y <- stu_elements[['plot_limits_round']][['max_y']]
  min_x <- stu_elements[['plot_limits_round']][['min_x']]
  max_x <- stu_elements[['plot_limits_round']][['max_x']]
  
  #3. put everything together
  p <- base_plot + 
    college_labels + 
    stu_elements[['plot_elements']][['line']] + 
    stu_elements[['plot_elements']][['points']] +
    stu_elements[['plot_elements']][['text']]
  
  #axis limits
  p <- p + 
    coord_cartesian(
      ylim = c(min_y - 1, max_y + 1)  
      ,xlim = c(min_x - 0.1, max_x + 0.1)
    ) +
    #format x
    scale_x_continuous(
      breaks = seq(min_x, max_x, by = 1) 
    ) +
    scale_y_continuous(
      breaks = seq(min_y, max_y, by = ifelse(max_y-min_y < 13, 2, round_any((max_y-min_y)/5, 5)))
    ) + 
    labs(
      x='Grade'
      ,y='RIT Score'
    )
  
  return(p)
}