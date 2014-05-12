# matches MAP data in long format season to season using dplyr
s2s_match <- function(.data, season1="Fall", season2="Spring", sy=2013){
  require(dplyr)
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


calc_quartile <- function(dtable, percentile.column = "TestPercentile", quartile.col.name=NULL){
  # This function takes a data.table, inspects its percentile colum and 
  # determins the recods's quartiel. A new data.table with an extra column 
  # containing the calculate quartile is returned
  #
  # Args:
  #   dtable:             a data.table with a column of pernctiles (as integers)
  #   percentile.column:  the name of the column in the data table as character
  #   quartile.col.name:  the name of the new quartile column
  #
  # Returns:
  #   dt: a datatable equivilent to the dtable argurment with an additional 
  #       column containing quartiles as integers (1:4) and named by 
  #       quartile.col.name argument
  
  if(is.null(quartile.col.name)) quartile.col.name<-"Quartile"
  
  if(!is.data.table(dtable)) dt<-as.data.table(dtable)
  else dt<-copy(dtable)
  pcn<-substitute(percentile.column)
  qcn<-substitute(quartile.col.name)
  
  dt[,c(qcn):=as.integer(NA)]
  
  dt[get(pcn)<25, 
     c(qcn):=1L]
  
  dt[get(pcn)>=25 & 
       get(pcn)<50, 
     c(qcn):=2L]
  
  dt[get(pcn)>=50 & 
       get(pcn)<75, 
     c(qcn):=3L]
  
  dt[get(pcn)>=75, 
     c(qcn):=4L]
  
  
  dt[,c(qcn):=as.factor(get(qcn))]
  
  
  dt
}

calc_tiered_growth<- function(dt, quartile.column, grade.column){
  # Function takes a data table and along with the names of the quartile indicator
  # and grade indicator columns and returns a data.table with the 1 extra column
  # providing the KIPP Foundations Tiered Growth guidelines. 
  #
  # Args:
  #   dt: a data.table with a quartile column and grade column
  #   quartile.column:  character vector of name of column containing student
  #                     quartiles
  #   grade.colum:      character vector of name of colum contain student
  #                     grade level
  #
  # Returns:
  #   dt: a data.table identical to the dt arguments data.table with the
  #       addition of a column containing the quartile column named
  #       KIPPTieredGrowth
  
  
  
  # Create data.table lookup of KIPP Foundation Growth Targts
  # using quartile.column name 
  tgrowth<-data.table(GradeType=c(rep(0,4),rep(1,4)), 
                      Quartile = as.factor(rep(1:4, 2)), 
                      KIPPTieredGrowth=c(1.5,1.5,1.25,1.25,2,1.75,1.5,1.25)
  )
  setkey(tgrowth, GradeType, Quartile)
  setnames(tgrowth, "Quartile", quartile.column)
  
  dt<-copy(dt)
  
  # Create Grade Type column
  dt[get(grade.column)<=3, GradeType:=0]
  dt[get(grade.column)>3, GradeType:=1]
  
  setkeyv(dt, c("GradeType", quartile.column))
  
  #merge data frames
  dt<-copy(tgrowth[dt])
  
  # Cleaning up 
  dt[,GradeType:=NULL]
  
  # TO DO: Reorder columns back to original data.table's ordering. 
  
  dt
}

orderid<-function(df,v1){
  # This function takes a dataframe df and a column name v1 used to sort the 
  # frame.  Returns a the dataframe with a new column OrderID which simply gives
  # the row numbers of the newly sorted datafram
  #
  # Args:
  #   df: a data frame wiht at leat one column
  #   v1: the name fo the column by which the data frame should be sorted and
  #       then ordered
  #
  # Returns:
  #   x:  the original dataframe with the an additional colum called OrderID, 
  #       which gives the column order
  #
  require(plyr)
  x<-arrange(df,get(v1))
  x$OrderID<-c(1:nrow(x))
  return(x)
}



