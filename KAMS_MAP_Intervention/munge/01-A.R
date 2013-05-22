# Preporcessing Script for KAMS Intervention Analysis 


#Need to want replace results stored as strings (i.e. "18%", "-23%") as numeric (i.e, 18, 23).  Need to loop through columns names and run gsub to remove punctuation.
anet.copy<-copy(anet.math.1112)

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

anet.math.1112<-copy(anet.copy)

anet.math.1112[,SASID:=as.factor(SASID)]

setkey(anet.math.1112,SASID)

rm(anet.copy)

########################
#  Get MAP Data Ready ##
########################


