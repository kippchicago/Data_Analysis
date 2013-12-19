get_MAPResults <- function(connection, season1 = "Fall13", season2=NULL){
  # This function attaches to an assessment results database and pulls data via
  #  JDBC from that database (given by connection.  The focus is on MAP data. If only season 1 is give, then it downloads the
  #  Entire dataset for that season.  If season2 is given (along with season1) then data is pulled only for 
  #  those students who took the test in seasons 1 and two
  #
  # Args:
  #     connection: a JDBC connection object 
  #     season1:  (required)  the first season to pull data for given in SeasonYEAR format (e.g. Fall13, Spring09, Winter11)
  #     season2:  The second season to pull data from, matched with students who took the assessment in prior season (season1)
  #
  # Returns: a dataframe with test results 
  #
  
  if(is.null(season2)) {
    qry<-sprintf(
      "SELECT   r.*,
      n.t42 as TypicalFallToSpringGrowth,
      n.r42 as ReportedFallToSpringGrowth,
      n.s42 as SDFallToSpringGrowth,
      n.t41 as TypicalFallToWinterGrowth,
      n.r41 as ReportedFallToWinterGrowth,
      n.s41 as SDFallToWinterGrowth,
      n.t44 as TypicalFallToFallGrowth,
      n.r44 as ReportedFallToFallGrowth,
      n.s44 as SDFallToFallGrowth,
      n.t22 as TypicalSpringToSpringGrowth,
      n.r22 as ReportedSpringToSpringGrowth,
      n.s22 as SDSpringToSpringGrowth,
      n.t12 as TypicalWinterToSpringGrowth,
      n.r12 as ReportedWinterToSpringGrowth,
      n.s12 as SDWinterToSpringGrowth
      FROM
      (
      SELECT   
      s.StudentLastName,
      s.StudentFirstName,
      s.Grade AS %s_Grade, 
      s.ClassName AS %s_ClassName,
      s.TeacherName AS %s_TeacherName,
      a.TermName,
      a.StudentID,
      a.SchoolName,
      a.MeasurementScale AS Subject,
      a.Discipline,
      a.GrowthMeasureYN,
      a.TestType,
      a.TestName,
      a.TestStartDate,
      a.TestDurationInMinutes,
      a.TestRITScore AS %s_RIT, 
      a.TestStandardError,
      a.TestPercentile AS %s_Pctl,
      a.RITtoReadingScore  AS %s_RITtoReadingScore,
      a.TestStartTime
      FROM   `tblAssessmentResults%s` a
      JOIN   (
      Select   st.*, c.ClassName, c.TeacherName
      FROM  tblStudentBySchool%s st
      JOIN  `tblClassAssignments%s` c
      ON st.StudentID=c.StudentID
      ) s
      ON 		s.StudentID=a.StudentID
      WHERE 	a.GrowthMeasureYN='TRUE'
      ) r
      LEFT OUTER JOIN `viewNorms2011_Growth_Kinder_0` n
      ON		r.`%s_RIT`=n.`StartRIT`
      AND		r.`%s_Grade`=n.`StartGrade2`
      AND		r.`Subject`=n.`MeasurementScale`
      ;
      ", season1, season1, season1, season1, season1, season1, season1, season1, season1, season1, season1)


  tryCatch(df<-dbGetQuery(connection, qry),
           error = function(w) {message(paste("You need to have a JDBC conncetion to the database.  Original error is:", w))}
  )
} 
else {
  
  qry <- sprintf("CALL GetMAPResultsFromToByName('%s', '%s');",season1, season2)
  tryCatch(df<-dbGetQuery(connection, qry),
           error = function(w) {print(paste("You need to have a JDBC conncetion to the database.  Original error is:", w))}
  )
  }
  df
}

abstractSeasonNames<- function(df, season1="Fall", season2="Spring", new1="Season1", new2="Season2"){
  # This functions allows user to change the names of variables returned by kippchidata db where 
  #  Season1YY_ is applied to variable names that difffer over seasons
  # Args:
  #       df: a dataframe
  #       season1, season2:  Variables season name prefix (i.e, Fall, Spring, Winter) as character string.  Double
  #                          digit years are automatically detected and removed by a regular expression
  #       new1, new2: new names for each season.  If used with lapply (the original use case for this function) seasondYY names 
  #                   will be the same across all dataframes in the list. 
  #
  # Returns:
  #       df: a datagrame with all varialbe or column names changed. All other variable names should be the same.
  #
  names(df)<-gsub(sprintf("(%s)(\\d\\d)(\\w+)", season1), sprintf("%s\\3",new1)  ,names(df))
  names(df)<-gsub(sprintf("(%s)(\\d\\d)(\\w+)", season2), sprintf("%s\\3",new2)  ,names(df))
  df
}





PrepMAP <- function (map.dt, season1, season2, growth.type="KIPP") {
  # Returns data.table wiht new columns indicating growth met, differene
  # in Spring and expected RIT scores, and indicators for earning above average
  # RIT scores in Fall and Spring.
  #
  # Args:
  #   map.dt: A data.table returned by a Call to the SQL procedure 
  #     GetMAPResultsFromToByName 
  #   season1:      the first season (i.e., test term) of the MAP assessment
  #                   as a character string
  #   season2:      the second season (i.e., test term) of the MAP assessment
  #                   as a character string
  #   growth.type:  either "KIPP" for 2013-14 KIPP Foundation college ready
  #                   growth or "Chicago" for KIPP Chicago 2012-13 college ready
  #                   growth.
  #
  #Returns:
  #   A data.table iwth addtional columns for Growth ProjectedGrowth, met NWEA
  #    growth, residual RIT (i.e., Actual Spring minus Expected Spring), and 
  #    indicators for above average RIT in Fall as well as Spring
  #   
  require(data.table)
  require(stringr)
  
  # Test that map.dt is a data table.  If not, issue a warning and 
  #  and coerce data.frame to data table

  #ERROR HANDLING
  if(!is.data.table(map.dt)){
    map.dt_name<-as.character(substitute(map.dt))
    map.dt_type<-class(map.dt)[1]
    warning(paste(map.dt_name, 
                  "is not a data.table.  Trying to coerce", 
                  map.dt_name, 
                  "from", 
                  map.dt_type, 
                  "to data.table."
                  ))
    map.dt<-tryCatch({
      m.dt<-as.data.table(map.dt)
      m.dt
      },
      warning = function(w) message(w),
      error = function(w) message(paste("An error was generated:\n", w)),
      finally = {
        stopifnot(is(m.dt, "data.table"))
        message(paste("Coercion of" , map.dt_name,"to data.table successful!"))
      }
    )
  }
  
  stopifnot(is(season1, "character"))
  stopifnot(is(season2, "character")|is(season2, "NULL"))
  stopifnot(growth.type=="KIPP"|growth.type=="Chicago")
  
  # END Error Handling
  
  season1.only<-is.null(season2)
  
  # Construct RIT score variable names
  RIT1<-paste(season1, "RIT", sep="_")
  if(!season1.only) RIT2<-paste(season2, "RIT", sep="_")
  
  # Construct Norm estimates variable names (i.e., season to season)
  s1 <- str_extract(season1, "[a-zA-Z]+")  # extracts season (letters) from 
                                           #  seasonYY 
  if(!season1.only) {
    s2 <- str_extract(season2, "[a-zA-Z]+")
  } else s2 <- "Spring"
  
  s1s2 <- paste0(s1,"To",s2)
  
  # Construct Growth Variable Name
  pgrowth.var<-paste0("Reported",s1s2,"Growth")
  
  # Calculate Projected Growth
  map.dt[,ProjectedGrowth:=get(RIT1)+get(pgrowth.var)]
  
  # Set indicator for exceeding NWEA Projected Growth 
  if(!season1.only){
    map.dt[get(RIT2)>=ProjectedGrowth,Meets:=1]
    map.dt[get(RIT2)<ProjectedGrowth,Meets:=0]
  
    # Calcualte difference Season2 score and Growth ProjectedGrowth
    map.dt[,TypicalGrowthDiff:=get(RIT2)-ProjectedGrowth]
  }
  #calculate quartiles 
  #construct Percentile column names
  Pctl1 <- paste(season1, "Pctl", sep="_")
  if(!season1.only) Pctl2 <- paste(season2, "Pctl", sep="_")
  
  Qrtl1 <- paste(season1, "Quartile", sep="_")
  if(!season1.only) Qrtl2 <- paste(season2, "Quartile", sep="_")
  
  map.dt<-do.call(calc_quartile,args=list(map.dt,Pctl1,Qrtl1))
  if(!season1.only) map.dt<-do.call(calc_quartile,args=list(map.dt,Pctl2,Qrtl2))
  
    
    
  if(growth.type=="Chicago"){
    # Calculate 75th Percentile Growth ProjectedGrowth
    # get z score (i.e., number of standard deviations) that corresponds to 75th 
    # percentile
    sigma<-qnorm(.75)
    
    # Constuct call for for typical growth (= acutal; reported, fwiw, = rounded)
    # and for hte standard deviation of growth
    tgrowth.var <- paste0("Typical",s1s2,"Growth")
    sd.var <- paste0("SD",s1s2,"Growth")
  
    # Constuct parsed term to evaluate
    gp75.var <- parse(text = paste0("GrowthPctl75th:=round(",
                                    tgrowth.var, 
                                    " + sigma*",
                                    sd.var,",0)"
                                    )
    )
  
    # add simga*SD to mean and round to integer
    map.dt[,eval(gp75.var)]
  
    # set "College Ready" ProjectedGrowth
    map.dt[,CollegeReadyGrowth:=get(RIT1)+GrowthPctl75th]
    
  } else if(growth.type=="KIPP"){
    map.dt <- do.call(calc_tiered_growth, args=list(map.dt, 
                                                    quartile.column=Qrtl1 ,
                                                    grade.column=paste(season1, 
                                                                       "Grade", 
                                                                       sep="_")
                                                    )
                      )
    # set "College Ready" ProjectedGrowth
    map.dt[,CollegeReadyGrowth:=get(RIT1)+KIPPTieredGrowth]
  }
  
  
  

  
  # Construct new indicator column name 
  # (see http://stackoverflow.com/questions/11745169/dynamic-column-names-in
  # -data-table-r?rq=1) for details on this eval(parse(text='sometext')) 
  # paradigm wiht data.tables
  
  if(!season1.only) a50th <- parse(text= paste0(season2, "_Above50th:=1"))
  if(!season1.only) b50th <- parse(text= paste0(season2, "_Above50th:=0"))
  
  #identify above/below 50th percenilte and add/update column
  if(!season1.only) map.dt[get(Pctl2)>=50, eval(a50th)]
  if(!season1.only) map.dt[get(Pctl2)<50, eval(b50th)]
    
  # Fall (see Spring above )
  
  a50th <- parse(text = paste0(season1, "_Above50th:=1"))
  b50th <- parse(text = paste0(season1, "_Above50th:=0"))
  
  map.dt[get(Pctl1)>=50, eval(a50th)]
  map.dt[get(Pctl1)<50, eval(b50th)]
  
  #Combine Student First and Last Names into one field
  
  map.dt[,StudentLastFirstName:=
           paste(StudentLastName, 
                 StudentFirstName, sep=", ")]
  
  map.dt[,StudentFirstLastName:=
           paste(StudentFirstName, 
                 StudentLastName, sep=" ")]
  
  # Add name with RIT score and Pctl in parenthesis. Will do so by constructing 
  # expression and then evaluating it within a data.table
  
  name.rit <- parse(text = paste0('StudentFirstLastNameRIT:=
                                  paste0(StudentFirstName, " ",
                                         StudentLastName, " ", 
                                         ' , season1,'_RIT, " (",
                                         ' , season1,'_Pctl, ")"
                                        )'
                                  )
                  )
                  
  
  map.dt[,eval(name.rit)]

  s1.ritpctl <- parse(text= paste0(season1,'_RITPctl:=
                                   paste0(', season1,'_RIT, " (",
                                          ', season1,'_Pctl, ")"
                                          )
                                   ')
  )
  
  
  map.dt[,eval(s1.ritpctl)]
  
  if(!season1.only) {
  s2.ritpctl <- parse(text= paste0(season2,'_RITPctl:=
                                   paste0(', season2,'_RIT, " (",
                                          ', season2,'_Pctl, ")"
                                          )
                                   ')
                      )
  
  
  map.dt[,eval(s2.ritpctl)]
  }
  # Reorder Grade level Factors
  
  # Make Grade a factor takes two steps
  # 1. Create expression to be evaluated in j term
  grade1<-parse(text = paste0(season1, 
                      '_Grade:=factor(', 
                      season1, 
                      '_Grade, levels=c("0", "1","2","3","4", "5", "6","7","8"))'))
  # 2. Evaluate expression in j term
  map.dt[,eval(grade1)]
  
  # 1. Create expression to be evaluated in j term
  if(!season1.only) {grade2<-parse(text = paste0(season2, 
                              '_Grade:=factor(', 
                              season2, 
                              '_Grade, levels=c("0", "1","2","3", "4", "5", "6","7","8"))'))
  # 2. Evaluate expression in j term
  map.dt[,eval(grade2)]
  }
  # Create expression for first term in setattr() function 
  refactor1<-parse(text = paste0("map.dt$",season1, "_Grade"))
  if(!season1.only) refactor2<-parse(text = paste0("map.dt$",season2, "_Grade"))
  
  # Relevel factors. 
  setattr(eval(refactor1), "levels", c("K", "1", "2", "3", "4", "5", "6","7","8"))
  if(!season1.only)  setattr(eval(refactor2), "levels", c("K", "1", "2", "3", "4", "5", "6","7","8"))
  
  # Change School Names to School Initials
  map.dt[SchoolName=="KIPP Ascend Primary School"
         |SchoolName=="KIPP Ascend Primary"
         |SchoolName=="KIPP ASCEND PRIMARY", 
         SchoolInitials:="KAP"]
  
  map.dt[SchoolName=="KIPP Ascend Middle School"
         |SchoolName==
           "KIPP Ascend Charter School                                       ", 
         SchoolInitials:="KAMS"]
  
  map.dt[SchoolName=="KIPP Create Middle School" | SchoolName=="KIPP Create College Prep", 
         SchoolInitials:="KCCP"]
  
  map.dt[SchoolName=="KIPP Bloom College Prep", 
         SchoolInitials:="KBCP"]
  
  map.dt[,SchoolInitials:=factor(SchoolInitials, 
                                 levels=c("KAP", "KAMS", "KCCP", "KBCP"))]
  
  
  
  map.dt
  
}
