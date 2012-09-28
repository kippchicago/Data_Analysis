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
    dftotal<-nrow(df)
    ddply(df, 
          grp, 
          function(X) data.frame(CountStudents=length(X[,RIT]), 
                                 AvgCountID=mean(X$OrderID), 
                                 AvgQrtlRIT=mean(X[,RIT]),
                                 PctofTotal=length(X[,RIT])/dftotal,
                                 TotalCount=dftotal
                                 )
          )
    }


#Function to report MAP RIT scores and goals (expected adn expected at 7th percentile) with summary stats
plot_MAP_Results_and_Goals <- function (df, plottitle=" ",labxpos=115, minx=105) {
  kippcols<-c("#E27425", "#FEBC11", "#255694", "A7CFEE")
  
  #Plot points for Fall RIT Score, Expected Growth, College Ready Growth, ordered by Fall RIT, Names on Y axis
  pointsize<-2
  p <- ggplot(df, aes(x=Fall12_RIT, y=OrderID)) +
    geom_text(aes(x=Fall12_RIT-1, color=as.factor(Quartile), label=StudentFirstLastName), size=2, hjust=1) +
    geom_point(aes(color=as.factor(Quartile)), size=pointsize) +
    geom_text(aes(x=Fall12_RIT+1, color=as.factor(Quartile), label=Fall12_RIT), size=2, hjust=0) +
    geom_point(aes(x=Fall12_RIT + TypicalFallToSpringGrowth, y=OrderID), color="#CFCCC1", size=pointsize) +
    geom_text(aes(x=Fall12_RIT + TypicalFallToSpringGrowth+1, label=round(Fall12_RIT + TypicalFallToSpringGrowth)), color="#CFCCC1", size=2, hjust=0) +
    geom_point(aes(x=GrowthTargets, y=OrderID), color="#FEBC11", size=pointsize) + 
    geom_text(aes(x=GrowthTargets+1, label=GrowthTargets), color="#FEBC11", size=2, hjust=0) +
    facet_grid(Quartile~., scale="free_y", space = "free_y", as.table=FALSE) +
    scale_colour_discrete(kippcols) + 
    scale_y_continuous(" ", breaks=df$OrderID, expand=c(0,1.5)) + 
    opts(axis.text.y = theme_text(size=3, hjust=1)) + 
    opts(legend.position = "none") + 
    scale_x_continuous("RIT Score") + 
    expand_limits(x=minx)+
    opts(
      panel.background = theme_rect(fill = "transparent",colour = NA), # or theme_blank()
      # panel.grid.minor = theme_blank(), 
      # panel.grid.major = theme_blank(),
      plot.background = theme_rect(fill = "transparent",colour = NA),
      axis.text.x = theme_text(size=15),
      axis.text.y = theme_blank(), 
      #axis.title.y = theme_blank(), 
      axis.ticks=theme_blank(),
      
      strip.text.x=theme_text(size=15),
      strip.text.y=theme_text(size=15,angle=0), 
      strip.background=theme_rect(fill="#F4EFEB", colour=NA),
      title=plottitle,
      plot.title=theme_text(size=12)
    ) 
  
  
  ###Let's add some summary labels by quaritle to p
  
  #First get the per panel data I want count by quartile, avg y-position (given by OrderID) by quartile,
  #  avg RIT by quartile, and percent of quartile students to total studens.
  
  qrtl.labels<-get_group_stats(df, grp="Quartile")
  
  #add a column with the actual label text
  qrtl.labels$CountLabel<-paste(qrtl.labels$CountStudents," students (",round(qrtl.labels$PctofTotal*100),"%)", sep="")
  
  qrtl.labels$AvgLabel<-paste("Avg RIT = ",round(qrtl.labels$AvgQrtlRIT))
  
  #eyeballed X position
  qrtl.labels$xpos<-rep(labxpos,nrow(qrtl.labels))
  
  #now adding this info to the plot p
  p <- p + geom_text(data=qrtl.labels, aes(x=xpos, y=AvgCountID, color=factor(Quartile),label=CountLabel),vjust=0, size=3.25) +
    geom_text(data=qrtl.labels, aes(x=xpos, y=AvgCountID, color=factor(Quartile),label=AvgLabel),vjust=1.5, size=3.25)
  
  p
}