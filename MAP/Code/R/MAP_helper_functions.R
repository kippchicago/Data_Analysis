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
plot_MAP_Results_and_Goals <- function (df, plottitle=" ",labxpos=115, minx=105,alp=1) {
  kippcols<-c("#E27425", "#FEBC11", "#255694", "A7CFEE")
  
  #Plot points for Fall RIT Score, Expected Growth, College Ready Growth, ordered by Fall RIT, Names on Y axis
  pointsize<-2
  p <- ggplot(df, aes(x=Fall12_RIT, y=OrderID)) +
    geom_text(aes(x=Fall12_RIT-1, color=as.factor(Quartile), label=StudentFirstLastName), size=2, hjust=1) +
    geom_point(aes(color=as.factor(Quartile)), size=pointsize) +
    geom_text(aes(x=Fall12_RIT+1, color=as.factor(Quartile), label=Fall12_RIT), size=2, hjust=0) +
    geom_point(aes(x=Fall12_RIT + TypicalFallToSpringGrowth, y=OrderID), color="#CFCCC1", size=pointsize,, alpha=alp) +
    geom_text(aes(x=Fall12_RIT + TypicalFallToSpringGrowth+1, label=round(Fall12_RIT + TypicalFallToSpringGrowth)), color="#CFCCC1", size=2, hjust=0, alpha=alp) +
    geom_point(aes(x=GrowthTargets, y=OrderID), color="#FEBC11", size=pointsize) + 
    geom_text(aes(x=GrowthTargets+1, label=GrowthTargets), color="#FEBC11", size=2, hjust=0) +
    facet_grid(Quartile~., scale="free_y", space = "free_y", as.table=FALSE) +
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
  
  RIT<-subset(kippdata, Subject==subj & Grade==grade)[,"Fall12_RIT"]
  ID<-rep(schoolname,length(RIT))
  Subject<-rep(subj,length(RIT))
  Grade<-rep(grade,length(RIT))
  
  df.kipp<-data.frame(RIT=RIT, ID=ID, Subject=Subject,Grade=Grade)
  
  df.combined<-rbind(df.kipp,df.norm)
  return(df.combined)
}


####And now for the small mulitples, comparative histograms

map_comparative_histograms <- function (df, legendpos="bottom", title=" ", schoolname="KAMS",...) {
  
  #get RIT score of 75th percentile of simulated national distributoin
  pctl75<-quantile(x=subset(df, ID=="National Norm")$RIT,probs=.75)
  
  bw<-2 #binning width
 

  
  
  #Adjust locationg of 75th percentile border (verticle line) for even and odd percentile scores
  if(pctl75%%2==0){
    pctl75.vline<-pctl75
    } else pctl75.vline<-pctl75 + 0.5*bw
  
  #Get means of simulated national and actual class distiribution for verticle line
  mean.vline<-c(mean(subset(df, ID==schoolname)$RIT),mean(subset(df, ID=="National Norm")$RIT))
  label.vline<-c(paste("Mean RIT = ", round(mean.vline[1]), sep=""), paste("Mean RIT = ", round(mean.vline[2]), sep=""))
  
  #find means of both distributions for verticles lines representing means in plots
  df.vline <- data.frame(ID=unique(df$ID), vl=mean.vline, mean.label=label.vline, p75=rep(pctl75,2), p75.label=rep(paste("75th %ile = ",round(pctl75),sep="")))
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

