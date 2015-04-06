library(data.table)

# Get ANet Data
anet.1112<-read.csv('/Users/chaid/Dropbox/Consulting/KIPP Ascend/Data Analysis/ANet/Results/KAMS_Students_results_201112.csv',na.strings="-")
anet.1112<-data.table(anet.1112)

#want to replace results stored as strings (i.e. "18%", "-23%") as numeric (i.e, 18, 23).  Need to loop through columns names and run gsub to remove punctuation.
anet.copy<-copy(anet.1112)

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

anet.1112<-copy(anet.copy)

anet.1112[,SASID:=as.factor(SASID)]

setkey(anet.1112,SASID)

#get MAP Data
drvr <- JDBC("com.mysql.jdbc.Driver","/Users/chaid/Dropbox/JDBC Drivers/mysql-connector-java-5.0.8/mysql-connector-java-5.0.8-bin.jar","")
con <- dbConnect(drvr,"jdbc:mysql://54.245.118.235/db_kippchidata", "chaid", "haiKIPP1" )

map.1112<-dbGetQuery(con, 'CALL GetMAPResultsFromTo("Fall11","Winter12");')

map.1112<-data.table(map.1112)
map.1112.math<-map.1112[Subject=="Mathematics"]

setkey(  )
#merge Data
map.anet.1112.math<-map.1112.math[anet.1112][,list(ID, StudentFirstName,  StudentLastName, FirstName, LastName, Fall11_Grade, Fall11_RIT, Fall11_Pctl, Spring12_RIT, Spring12_Pctl, AVERAGE_School, DIFFERENCE_Network, Interim_1_School, Interim_1_Network, Interim_2_School, Interim_2_Network, Interim_3_School, Interim_3_Network)]


#Fit logistic regrssion 
fit.1<-glm(SpringAbove50 ~ Fall11_RIT + Interim_1_Network + Interim_2_Network + Interim_3_Network + as.factor(Fall11_Grade), family=binomial(link="logit"), data = map.anet.1112.math)


for (i in 1:nrow(coef(fit.1.sim))){
  curve(coef(fit.1.sim)[i,1] + coef(fit.1.sim)[i,2]*x + 
          coef(fit.1.sim)[i,3]*map.anet.1112.math[Fall11_Grade==8,mean(Interim_1_Network)] + 
          coef(fit.1.sim)[i,4]*map.anet.1112.math[Fall11_Grade==8,mean(Interim_2_Network)] + 
          coef(fit.1.sim)[i,4]*map.anet.1112.math[Fall11_Grade==8,mean(Interim_3_Network)] + 
          coef(fit.1.sim)[i,8], 
        add=TRUE, col="gray", alpha=.2) 
}


setkey(map.1112.math, ID)