PrepANet <- function (anet.df) {
  # Prepare ANet Data to be merged with MAP Data
  #
  # Args:
  #   anet.df: A data.table with created from an ANet csv file.
  #
  # Returns: A data.table with punction fixed and ready to merge with MAP data
  anet.copy<-copy(anet.df)
  
  setnames(anet.copy,gsub("[[:punct:]]","_",names(anet.copy)))
  
  s<-which(names(anet.copy)=="AVERAGE_School")
  
  cnames <- names(anet.copy)[s:ncol(anet.copy)]
  
  for(cname in cnames){
    anet.copy[,
              cname := as.numeric(gsub("([[:digit:]]+)([[:punct:]])",
                                       "\\1",
                                       as.character(anet.copy[[cname]]))),
              with=FALSE]
  }
  
  
  #separate names into two columns
  x<-unlist(strsplit(as.character(anet.copy[,Student]),", "))
  dim(x)<-c(2,length(x)/2)
  x<-t(x)
  anet.copy[, c("FirstName","LastName") := list(x[,1],x[,2]), with=FALSE]
  
  
  
  anet.copy[,SASID:=as.factor(SASID)]
  
  setkey(anet.copy,SASID)
  
  anet.copy
}

PrepMAP <- function (map.dt, season1, season2) {
  # Returns data.table wiht new columns indicating growth met, differene
  # in Spring and expected RIT scores, and indicators for earning above average
  # RIT scores in Fall and Spring.
  #
  # Args:
  #   map.dt: A data.table returned by a Call to the SQL procedure 
  #     GetMAPResultsFromToByName 
  #   season1: the first season (i.e., test term) of the MAP assessment
  #   season2: the second season (i.e., test term) of the MAP assessment
  #
  #Returns:
  # map.dt: A data.table iwth addtional columns for Growth ProjectedGrowth, met NWEA
  #   growth, residual RIT (i.e., Actual Spring minus Expected Spring), and 
  #   indicators for above average RIT in Fall as well as Spring
  #   
  require(data.table)
  require(stringr)
  
  # Construct RIT score variable names
  RIT1<-paste(season1, "RIT", sep="_")
  RIT2<-paste(season2, "RIT", sep="_")
  
  # Construct Norm estimates variable names (i.e., season to season)
  s1 <- str_extract(season1, "[a-zA-Z]+")  # extracts season (letters) from 
                                           #  seasonYY 
  s2 <- str_extract(season2, "[a-zA-Z]+")
  
  s1s2 <- paste0(s1,"To",s2)
  
  # Construct Growth Variable Name
  pgrowth.var<-paste0("Reported",s1s2,"Growth")
  
  # Calculate Projected Growth
  map.dt[,ProjectedGrowth:=get(RIT1)+get(pgrowth.var)]
  
  # Set indicator for exceeding NWEA Projected Growth 
  map.dt[get(RIT2)>=ProjectedGrowth,Meets:=1]
  map.dt[get(RIT2)<ProjectedGrowth,Meets:=0]
  
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
    
  # Calcualte difference Season2 score and Growth ProjectedGrowth
  map.dt[,TypicalGrowthDiff:=get(RIT2)-ProjectedGrowth]
  
  
  # Set indicator for student performing above Natioanl Norm Average (2011
  # Norms) (i.e., at or above 50th Percentile)
  
  #construct Percentile column names
  Pctl1 <- paste(season1, "Pctl", sep="_")
  Pctl2 <- paste(season2, "Pctl", sep="_")
  
  # Construct new indicator column name 
  # (see http://stackoverflow.com/questions/11745169/dynamic-column-names-in
  # -data-table-r?rq=1) for details on this eval(parse(text='sometext')) 
  # paradigm wiht data.tables
  
  a50th <- parse(text= paste0(season2, "_Above50th:=1"))
  b50th <- parse(text= paste0(season2, "_Above50th:=0"))
  
  #identify above/below 50th percenilte and add/update column
  map.dt[get(Pctl2)>=50, eval(a50th)]
  map.dt[get(Pctl2)<50, eval(b50th)]
    
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
  
  # Add name with RIT score in parenthesis. Will do so by constructing 
  # expression and then evaluating it within a data.table
  
  name.rit <- parse(text = paste0('StudentFirstLastNameRIT:=
                                  paste0(StudentFirstName, " ",
                                         StudentLastName, " ", 
                                         ' , season1,'_RIT
                                        )'
                                  )
                  )
                  
  
  map.dt[,eval(name.rit)]
  
  
  # Reorder Grade level Factors
  
  # Make Grade a factor takes two steps
  # 1. Create expression to be evaluated in j term
  grade1<-parse(text = paste0(season1, 
                      '_Grade:=factor(', 
                      season1, 
                      '_Grade, levels=c("0", "1","2", "5", "6","7","8"))'))
  # 2. Evaluate expression in j term
  map.dt[,eval(grade1)]
  
  # Create expression for first term in setattr() function 
  refactor1<-parse(text = paste0("map.dt$",season1, "_Grade"))
  
  # Relevel factors. 
  setattr(eval(refactor1), "levels", c("K", "1", "2", "5", "6","7","8"))
  
  # Change School Names to School Initials
  map.dt[SchoolName=="KIPP Ascend Primary"|SchoolName=="KIPP ASCEND PRIMARY", SchoolInitials:="KAPS"]
  map.dt[SchoolName=="KIPP Ascend Middle School"
         |SchoolName==
           "KIPP Ascend Charter School                                       ", 
         SchoolInitials:="KAMS"]
  
  map.dt[SchoolName=="KIPP Create Middle School", 
         SchoolInitials:="KCCP"]
  
  map.dt[,SchoolInitials:=factor(SchoolInitials, 
                                 levels=c("KAPS", "KAMS", "KCCP"))]
  
  
  
  map.dt
  
}
