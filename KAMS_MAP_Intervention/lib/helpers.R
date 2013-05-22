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
