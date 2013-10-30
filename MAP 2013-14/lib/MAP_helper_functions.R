# MAP Analsyis Helper Functions, based on MAP Analysis Helper Function of Fall
# 2013
# Spring 2014, Christopher J Haid and KIPP Chicago

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
  
  dt[get(pcn)>75, 
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
                      Quartile = rep(1:4, 2), 
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
  dt<-copy(dt[tgrowth])
  
  # Cleaning up 
  dt[,GradeType:=NULL]
  
  # Reorder columns back to original data.table's ordering. 
  
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



#Function to get count, pct of total, average x and avearge y 
get_group_stats<-function(df, grp="Quartile",RIT="Fall13_RIT"){
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
plot_waterfall <- function (df, 
                            plottitle=" ", 
                            season1="Fall13", 
                            season2=NULL, 
                            tiered.growth="KIPPTieredGrowth", 
                            labxpos=115, 
                            minx=105,
                            alp=1) {
  
  # Test if this is a two season (with arrows) or one season (with dots)
  if(is.null(season2)) season1.only<-TRUE
  
  #assinge kipp colors for quartiles
  kippcols<-c("#E27425", "#FEBC11", "#255694", "A7CFEE")
  
  
  #get names for all season specific variables to be used in 
  season1.rit<-sprintf("%s_RIT", season1)
  
  
  season1.quartile<-sprintf("%s_Quartile", season1)
  season1.grade<-sprintf("%s_Grade", season1)
  season1.class<-sprintf("%s_Classname", season1)
  
  if(!season1.only){
    season2.rit<-sprintf("%s_RIT", season2)
    season2.quartile<-sprintf("%s_Quartile", season2)
    season2.grade<-sprintf("%s_Grade", season2)
    season2.class<-sprintf("%s_Classname", season2)
    typical.growth<-sprintf("Reported%sTo%sGrowth",
                            str_extract(season1, "[[:alpha:]]+"),
                            str_extract(season2, "[[:alpha:]]+")
    )
  } else typical.growth<-"ReportedFallToSpringGrowth"
  
  #college ready target = typical growth *  tiered growth multiplier 
  cr.growth<-paste0(typical.growth, "*", tiered.growth)
  
  df<-copy(df)
  
  #change name of RIT Score column if it is equal to TestRITScore
  if(!exists(season1.rit, df)) setnames(df, "TestRITScore", season1.rit)
  
  
  
  #check for orderid and add if not existent
  if(!exists("OrderID", df)){
    order.id <- function(s, d){
      d2<-copy(d)
      d2<-eval(substitute(d2[order(get(s)), 
                             OrderID:=c(1:.N), 
                             by=list(Grade, MeasurementScale)]
      )
      )
      d2
    }
    df <- order.id(season1.rit, df)
  }
  
  
  
  
  
  
  #Plot points for Fall RIT Score, Expected Growth, College Ready Growth, ordered by Fall RIT, Names on Y axis
  pointsize<-2
  p <- ggplot(df, aes_string(x=season1.rit, y="OrderID")) 
  
  
  # Need to check for each growth categories existence, otherise you can throuhg
  # a pretty annoying error (which was happily discovered when two grade-subject
  # pairs had no negative growth in 2012-13)
  
  # Get extant growth categories (only if season1.only=FALSE)
  if(!season1.only){
    growth.cat<-unique(df[,GrowthCat])
    
    if("College Ready" %in% growth.cat){
      p <- p + geom_segment(data=df[GrowthCat=="College Ready"], 
                            aes_string(x=season1.rit,
                                       xend=season2.rit, 
                                       y="OrderID", 
                                       yend="OrderID"), 
                            arrow = arrow(length = unit(0.1,"cm")), 
                            color="#FEBC11") 
    }
    
    if("Typical" %in% growth.cat){
      p <- p + geom_segment(data=df[GrowthCat=="Typical"], 
                            aes_string(x=season1.rit, 
                                       xend=season2.rit, 
                                       y="OrderID", 
                                       yend="OrderID"), 
                            arrow = arrow(length = unit(0.1,"cm")), 
                            color="#CFCCC1") 
    }
    
    if("Positive" %in% growth.cat){
      p <- p + geom_segment(data=df[GrowthCat=="Positive"], 
                            aes_string(x=season1.rit, 
                                       xend=season2.rit, 
                                       y="OrderID", 
                                       yend="OrderID"), 
                            arrow = arrow(length = unit(0.1,"cm")), 
                            color="#C49A6C") 
    }
    
    if("Negative" %in% growth.cat){
      p <- p + geom_segment(data=df[GrowthCat=="Negative"], 
                            aes_string(x=season1.rit, 
                                       xend=season2.rit, 
                                       y="OrderID", 
                                       yend="OrderID"),  
                            arrow = arrow(length = unit(0.1,"cm")), 
                            color="red") 
      
      p <- p + geom_text(data=df[GrowthCat=="Negative"],
                         aes_string(x="season2.rit-.5", 
                                    color="as.factor(season2.quartile)", 
                                    label="season2.rit"), 
                         size=2, 
                         hjust=1) 
      p <- p + geom_text(data=df[GrowthCat=="Negative"],
                         aes_string(x="season1.rit+1", 
                                    color="as.factor(season1.quartile)", 
                                    label="season1.rit"), 
                         size=2, 
                         hjust=0) 
      
      p <- p + geom_text(data=df[GrowthCat=="Negative"],
                         aes_string(x="season2.rit-4", 
                                    label="StudentFirstLastName"), 
                         color="red" ,
                         size=2, 
                         hjust=1) 
    }
    
    p <- p +  geom_text(data=df[GrowthCat!="Negative"],
                        aes_string(x="season2.rit+.5", 
                                   color="as.factor(season2.quartile)", 
                                   label="season2.rit"), 
                        size=2, 
                        hjust=0) 
    
    
    
    p <- p + geom_text(data=df[GrowthCat!="Negative"],
                       aes_string(x="season1.rit.rit-1", 
                                  color="as.factor(season1.quartile)", 
                                  label="StudentFirstLastNameRIT"), 
                       size=2, 
                       hjust=1) 
    
    p <- p + geom_text(data=df[GrowthCat=="Positive"],
                       aes_string(x="season1.rit-1",  
                                  label="StudentFirstLastNameRIT"), 
                       size=2, 
                       hjust=1,
                       color="#C49A6C") 
    
  }
  else {
    #for names if season1 only. 
    name.x <- paste0(season1.rit, "-1")
    name.label <- sprintf(
      'paste0(StudentFirstName, " ", StudentLastName, " ", %s)',
      season1.rit)
    
    p <- p + geom_text(aes_string(x=name.x,  
                                  label=name.label,
                                  color=season1.quartile), 
                       size=2, 
                       hjust=1
    ) 
    
    
  }
  
  #labels and other string tricks
  tg.target<-paste0(season1.rit, "+", typical.growth)
  cr.target<-paste0(season1.rit, "+", cr.growth)
  
  tg.label <- paste0("round(",tg.target,")")
  cr.label <- paste0("round(",cr.target,")")
  
  facet.formula<-as.formula(paste0(season1.quartile,"~."))
  # Season 1 Point   
  
  p <- p + geom_point(aes_string(color=season1.quartile), 
                      size=pointsize)
  # Typical Growth
  if(season1.only) dot=19 else dot="|" 
  p<- p + geom_point(aes_string(x=tg.target, 
                                y="OrderID"), 
                     color="#CFCCC1", 
                     size=pointsize-.5, 
                     alpha=alp, 
                     shape=dot) +
    geom_text(aes_string(x=tg.target, label=tg.label),
              color="#CFCCC1",
              size=pointsize-.5, 
              hjust=1, 
              vjust=-1, 
              alpha=alp) +
    geom_point(aes_string(x=cr.target, 
                          y="OrderID"), 
               color="#FEBC11", 
               size=pointsize-.5, 
               alpha=alp, 
               shape=dot) + 
    geom_text(aes_string(x=cr.target, 
                         label=cr.label), 
              color="#FEBC11", 
              size=pointsize-.5, 
              hjust=0, 
              vjust=-1, 
              alpha=alp) +
    facet_grid(facet.formula, 
               scale="free_y", 
               space = "free_y", 
               as.table=FALSE) +
    scale_colour_discrete(kippcols) + 
    scale_y_continuous(" ", breaks=df$OrderID, expand=c(0,1.5)) + 
    theme(axis.text.y = element_text(size=3, hjust=1)) + 
    theme(legend.position = "none") + 
    scale_x_continuous("RIT Score") + 
    expand_limits(x=minx)+
    theme(
      panel.background = element_rect(fill = "transparent",colour = NA), # or element_blank()
      # panel.grid.minor = element_blank(), 
      # panel.grid.major = element_blank(),
      plot.background = element_rect(fill = "transparent",colour = NA),
      axis.text.x = element_text(size=15),
      axis.text.y = element_blank(), 
      #axis.title.y = element_blank(), 
      axis.ticks=element_blank(),
      
      strip.text.x=element_text(size=15),
      strip.text.y=element_text(size=15,angle=0), 
      strip.background=element_rect(fill="#F4EFEB", colour=NA),
      plot.title=element_text(size=12)
    ) +
    ggtitle(plottitle)
  
  
  ###Let's add some summary labels by quaritle to p
  
  #First get the per panel data I want count by quartile, avg y-position (given by OrderID) by quartile,
  #  avg RIT by quartile, and percent of quartile students to total studens.
  
  s1qrtl.labels<-as.data.table(get_group_stats(as.data.frame(df), 
                                               grp=season1.quartile))
  #add a column with the actual label text
  
  s1qrtl.labels[,CountLabel:=paste(s1qrtl.labels$CountStudents,
                                   " students (",
                                   round(s1qrtl.labels$PctofTotal*100),"%)", 
                                   sep="")]
  
  s1qrtl.labels[,AvgLabel:=paste(sprintf("%s Avg RIT", 
                                         str_extract(season1, 
                                                     "[[:alpha:]]")),
                                 round(s1qrtl.labels[,AvgQrtlRIT]))]
  #eyeballed X position
  s1qrtl.labels[,xpos:=rep(labxpos,nrow(s1qrtl.labels))]
  
  # lbaels as above for season 2   
  if(!season1.only){   
    s2qrtl.labels<-as.data.table(get_group_stats(as.data.frame(df), 
                                                 grp=season2.quartile))
    s2qrtl.labels[,CountLabel:=paste(s2qrtl.labels$CountStudents,
                                     " students (",
                                     round(s2qrtl.labels$PctofTotal*100),"%)", 
                                     sep="")]
    
    
    
    s2qrtl.labels[,AvgLabel:=paste(sprintf("%s Avg RIT", 
                                           str_extract(season2, 
                                                       "[[:alpha:]]")),
                                   round(s2qrtl.labels[,AvgQrtlRIT]))]
    
    # realigning to the proper facet
    setnames(s2qrtl.labels, old=1, new=season1.quartile)
    
    s2qrtl.labels[season1.quartile %in% unique(
      s1qrtl.labels[,season1.quartile]),
                  AvgCountID:=s1qrtl.labels[,AvgCountID]-5]
    
    s2qrtl.labels[,xpos:=rep(labxpos,nrow(s2qrtl.labels))]
    
  }
  
  
  #now adding this info to the plot p
  p <- p + geom_text(data=s1qrtl.labels, 
                     aes_string(x="xpos", 
                                y="AvgCountID", 
                                color=season1.quartile,
                                label="CountLabel"),
                     vjust=0, 
                     size=3.25) +
    geom_text(data=s1qrtl.labels, 
              aes_string(x="xpos", y="AvgCountID", 
                         color=season1.quartile,
                         label="AvgLabel"),
              vjust=1.5, 
              size=3.25) 
  if(!season1.only){  
    p <- p + geom_text(data=s2qrtl.labels, 
                       aes_string(x="xpos", 
                                  y="AvgCountID", 
                                  color=season1.quartile,
                                  label="CountLabel"),
                       vjust=0, 
                       size=3.25) +
      geom_text(data=s2qrtl.labels, 
                aes_string(x="xpos", 
                           y="AvgCountID", 
                           color=season1.quartile,
                           label="AvgLabel"),
                vjust=1.5, 
                size=3.25) 
  }
  p
}

##This function provides data for national refernce histogram

map_2011_simulated_distr <- function (m=212.9,s=14.8) {
  
  set.seed(120911) # for replicability's sake
  
  U<-280 #upper limit on RIT scale (I've imposed this)
  L<-120 #lower limit on RIT scale (I've imposed this)
  n<-5000
  
  #m<-212.9 #5th Grade Math Mean, Fall 2011 Norms
  #s<-14.8 #5th Grade Math SD, Fall 2011 Norms
  
  #need to get cumulative probablity of upper and lower bounds from normal CDF with mean m and sd s
  p_U<-pnorm(U, mean=m, sd=s)
  p_L<-pnorm(L, mean=m, sd=s)
  
  #1. draw 1000 rv distributed uniform between p_L and p_U.
  #2. Compute the reverse CDF (normal) to get distribution of scores  
  df.norm.ref<-data.frame(as.numeric(qnorm(runif(n,p_L,p_U),mean=m,sd=s)),rep("National Norm", n))
  
  names(df.norm.ref)<-c("RIT", "ID")
  df.norm.ref$RIT<-round(df.norm.ref$RIT,0) #round to integer
  return(df.norm.ref)
}

###Combin KIPP and National Distr data in a dataframe.  Norms data must have fields 'Mean', 'SD', 'Grade' and 'Subject'.

map_combined_histo_data <- function (kippdata=map.scores.KAMS, normsdata=nwea.norms.fall, grade=5, subj="Mathematics", schoolname="KAMS") {
  df.norm<-map_2011_simulated_distr(m=subset(normsdata, Grade==grade & Subject==subj)$Mean,s=subset(normsdata, Grade==grade & Subject==subj)$SD)
  #get g Grade Data by schools 
  df.norm$Subject<-subj
  df.norm$Grade<-grade
  
  
  df<-subset(kippdata, Subject==subj & Grade==grade)
  df<-arrange(df, SchoolName) #make sure that all students are sperated by school after subsetting
  RIT<-df[,"season1.rit"]
  l<-length(schoolname)
  if(l>1){
    kippnames<-unique(kippdata$SchoolName)
    l.name1<-nrow(subset(df, SchoolName==kippnames[1]))
    l.name2<-nrow(subset(df, SchoolName==kippnames[2]))
    
    ID1<-rep(schoolname[1],l.name1)
    ID2<-rep(schoolname[2],l.name2)
    
    ID<-c(ID1,ID2)
    
  }
  else ID<-rep(schoolname,length(RIT))
  
  Subject<-rep(subj,length(RIT))
  Grade<-rep(grade,length(RIT))
  
  df.kipp<-data.frame(RIT=RIT, ID=ID, Subject=Subject,Grade=Grade)
  
  df.combined<-rbind(df.kipp,df.norm)
  return(df.combined)
}


####And now for the small mulitples, comparative histograms

map_comparative_histograms <- function (df, legendpos="bottom", title=" ",...) {
  
  #get RIT score of 75th percentile of simulated national distributoin
  pctl75<-quantile(x=subset(df, ID=="National Norm")$RIT,probs=.75)
  
  bw<-2 #binning width
 

  
  
  #Adjust locationg of 75th percentile border (verticle line) for even and odd percentile scores
  if(pctl75%%2==0){
    pctl75.vline<-pctl75
    } else pctl75.vline<-pctl75 + 0.5*bw
  
  #Get means of simulated national and actual class distiribution for verticle line
  mean.vline<-label.vline<-vector()
  for(n in unique(df$ID)){
    school.mean<-mean(subset(df, ID==n)$RIT)
    mean.vline<-c(mean.vline,school.mean)
    label.vline<-c(label.vline, paste("Mean RIT = ", round(school.mean), sep=""))
  }
      
  #this whole section is hacky.  Is shcould probably put abbreviations in original data set
  
  #find means of both distributions for verticles lines representing means in plots
  df.length<-length(mean.vline)
  df.vline <- data.frame(ID=unique(df$ID), vl=mean.vline, mean.label=label.vline, p75=rep(pctl75,df.length), p75.label=rep(paste("75th %ile = ",round(pctl75),sep=""),df.length))
  #KIPP friendly colors
  kippcols <- c("#CFCCC1","#60A2D7")
  
  #these next two lines allow me to pass the variable pctl75 to geom_hist, rather than hardcoding anything!
  fill.text<-paste("..x..>",pctl75,sep="")
  y.text<-"..density.."
  
  p <- ggplot(df, aes(x=RIT)) 
  p <- p + geom_histogram(aes_string(y = y.text, fill=fill.text),  binwidth=bw)  + 
    facet_grid(ID~.) +
    geom_vline(x=pctl75.vline, color="#E27425") +
    geom_text(data=df.vline, aes(x=p75+1, y=.05, label=p75.label), color="#E27425", size = 3, hjust=0) +
    geom_vline(data=df.vline, aes(xintercept=vl), color="#439539") +
    geom_text(data=df.vline, aes(x=vl-1, y=.05, label=mean.label), color="#439539", size = 3, hjust=1) +
    scale_fill_manual("College Readiness",values = kippcols, labels=c("< 75th Percentile", "> 75th Percentile")) +
    theme(legend.position=legendpos) + 
    ggtitle(title)
  p
}

pdf_waterfall <- function(.data, school, season){
  
  # check that .data is a data.table
  if(!is.data.table(.data)) stop(".data must be data.table.")
  
  dtable <- copy(.data)
  # Check if SchoolInitials exists; if not, create column 
  if(!exists("SchoolInitials", dtable)){
    if(exists("SchoolName", dtable)){
      dtable[,SchoolInitials:=gsub("(^|\\s)([A-Z])([A-Za-z]+)", "\\2", SchoolName)]
    } else stop("Need either a SchoolInitials column or a SchoolName column.")
  }
  dtable<-dtable[SchoolInitials==school]
  
  todays.date<-format(Sys.time(), "%y%m%d")
  
  pdf(file=paste0("graphs/",season,"_MAP_",school,"_",todays.date,".pdf"), 
      height=10.5, 
      width=8)
  
  for(s in sort(unique(dtable[SchoolInitials==school, MeasurementScale]))){
    for(k in unique(dtable[SchoolInitials==school,SchoolInitials])){
      dfp<-dtable[MeasurementScale==s 
                  & SchoolInitials==k, ] #Datatable to Plot
      for(g in as.character(sort(unique(dfp[,Grade])))){
        ptitle <- paste0(k, 
                         " Fall 2013 MAP Scores ", 
                         g," ",
                         s, 
                         "\nFall RIT Scores with Typical Growth and College Ready Growth\nby Fall Quartile")
        p<-plot_waterfall(dfp[Grade==g,], 
                          ptitle, 
                          labxpos=100, 
                          minx=95,
                          alp=1)
        print(p)
      }
    }  
  }
  dev.off()
}


