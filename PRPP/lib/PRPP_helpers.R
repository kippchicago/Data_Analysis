orderid<-function(df,v1){
  # Function sourts data frame df1 by column v1 and then adds column OrderID 
  # giving row sequence.
  # Args:
  #   df: a dataframe or data.table
  #   v1: the column name by which to sort and sequence
  #
  # Returns:
  #   The origianl data frame with the an additional column, OrderID, giveing 
  #   the sequential order
  #
  if(!require(plyr)){
    print("Function OrderID requires package plyr")
  }
  x<-arrange(df,v1)
  x$OrderID<-c(1:nrow(x))
  return(x)
}

arrowdiagr<-function(data, t=" ", ranking="Fall", schoolname="KIPP Ascend Charter School", masked=FALSE){
  require(grid)
  X<-NA
  rvar<-NA
  X<-data
  comment(X)<-"X"  #provides a hacked means to use X in the paste on the next line

    
  if(ranking=="Fall"|ranking=="Spring"){
    
    rvar <- paste(comment(X),"$",ranking,".RIT",sep="") 
    
    X<-orderid(X,eval(parse(text=rvar))) 
    
    # Mask other school names if masked == TRUE
    if(masked) {
      mask <- parse(text = paste0("School_Display_Name!= '",schoolname,"'"))
      X[eval(mask), School_Display_Name:= paste0('KIPP School ',OrderID)]
    }
    
    p <- ggplot(X, aes(x=Fall.RIT, y=OrderID)) + geom_segment(aes(xend=Spring.RIT, yend=OrderID, color=-posdiff),size=3, arrow=arrow(ends="last", length=unit(0.4, "cm"), type= "open", angle=20))
    p <- p + geom_segment(data=X[X$School_Display_Name==schoolname,],aes(x=Fall.RIT, y=OrderID, xend=Spring.RIT, yend=OrderID), color="#439539", size=4, arrow=arrow(length=unit(0.4, "cm"), type= "open", angle=20))
    p <- p  + scale_y_continuous(" ", breaks=X$OrderID, labels=X$School_Display_Name)
    p <- p + theme(axis.text.y = element_text(size=7, hjust=1))
    p <- p + labs(title=t)
    p <- p + theme(legend.position = "none")
    p <- p + scale_x_continuous("RIT Score")
  }
  
  if(ranking=="diff"){
    rvar <- paste(comment(X),"$",ranking,sep="") 
    
    X<-orderid(X,eval(parse(text=rvar))) 
    
    # Mask other school names if masked == TRUE
    if(masked) {
      mask <- parse(text = paste0("School_Display_Name!= '",schoolname,"'"))
      X[eval(mask), School_Display_Name:= paste0('KIPP School ',OrderID)]
    }
    
    p <- ggplot(X, aes(x=0, y=OrderID)) + geom_segment(aes(xend=diff, yend=OrderID, color=-posdiff),size=3, arrow=arrow(ends="last", length=unit(0.4, "cm"), type= "open", angle=20))
    p <- p + geom_segment(data=X[X$School_Display_Name==schoolname,],aes(x=0, y=OrderID, xend=diff, yend=OrderID), color="#439539", size=3, arrow=arrow(length=unit(0.4, "cm"), type= "open", angle=20))
    p <- p  + scale_y_continuous(" ", breaks=X$OrderID, labels=X$School_Display_Name)
    p <- p + theme(axis.text.y = element_text(size=7, hjust=1))
    p <- p + labs(title=t)
    p <- p + theme(legend.position = "none")
    p <- p + scale_x_continuous("Fall to Spring Change  in RIT Score")
  } 
  if(ranking=="PctGrowth"){
    rvar <- paste(comment(X),"$","Percent_Met_Growth_Target",sep="") 
    
    X<-orderid(X,eval(parse(text=rvar))) 
    
    # Mask other school names if masked == TRUE
    if(masked) {
      mask <- parse(text = paste0("School_Display_Name!= '",schoolname,"'"))
      X[eval(mask), School_Display_Name:= paste0('KIPP School ',OrderID)]
    }
    
    p <- ggplot(X, aes(x=0, y=OrderID)) + geom_segment(aes(xend=Percent_Met_Growth_Target, yend=OrderID), color="#60A2D7",size=3)
    p <- p + geom_segment(data=X[X$School_Display_Name==schoolname,],aes(x=0, y=OrderID, xend=Percent_Met_Growth_Target, yend=OrderID), color="#439539", size=3)
    p <- p + geom_text(aes(x=Percent_Met_Growth_Target + 1, y=OrderID, label=Percent_Met_Growth_Target), hust=0, size=3)
    p <- p  + scale_y_continuous(" ", breaks=X$OrderID, labels=X$School_Display_Name)
    p <- p + theme(axis.text.y = element_text(size=7, hjust=1))
    p <- p + labs(title=t)
    p <- p + theme(legend.position = "none")
    p <- p + scale_x_continuous("% Meeting Growth Target")
  } 	
  p
}