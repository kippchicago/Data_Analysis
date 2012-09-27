###################################
###################################
## MAP Analsyis Helper Functions ##
## Fall 2012, Christopher J Haid ##
###################################
###################################
#This function takes a dataframe df and a column name v1 used to sort the frame.  Returns a the dataframe
#with a new column OrderID which simply gives the row numbers of the newly sorted datafram
orderid<-function(df,v1){
  require(plyr)
  x<-arrange(df,v1)
  x$OrderID<-c(1:nrow(x))
  return(x)
}



#Function to get count, pct of total, average x and avearge y 
get_group_stats<-function(df, grp="Quartile",RIT="Fall12_RIT"){
    require(plyr)
    ddply(df, 
          grp, 
          function(X) data.frame(CountStudents=length(X[,RIT]), 
                                 AvgCountID=mean(X$OrderID), 
                                 AvgQrtlRIT=mean(X[,RIT])
                                 )
          )
    }