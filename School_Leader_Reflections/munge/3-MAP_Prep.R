# Preporcessing Script for KAMS Intervention Analysis 



########################
#  Get MAP Data Ready ##
########################

#  Need to remove Monique Dellar from 1st to Kinder and remove the extra
# record fo hers (She is currently in K Notre Dame)
map.1213<-copy(map.1213[!(ID==50206087 & Fall12_ClassName=="Michigan")][ID==50206087,Fall12_Grade:=0])


#Now prep data
map.1213<-PrepMAP(map.1213, "Fall12", "Spring13")






# Prep data for Season to Season Arrow Charts

# Sort scores scores by grade
map.scores.by.grade<-ddply(map.1213, .(Subject, SchoolName, Fall12_Grade), function(df) orderid(df,"Fall12_RIT"))

# Make data.table for easier subsetting and assignment
map.scores.by.grade<-data.table(map.scores.by.grade)

#Relevel subject factors 
setattr(map.scores.by.grade$Subject, "levels", c("Mathematics", "Reading", "Language Usage", "General Science"))

#set growth Categories (note order matters here to get all the categories right)

map.scores.by.grade[Spring13_RIT<Fall12_RIT, GrowthCat:="Negative"]
map.scores.by.grade[Spring13_RIT>=Fall12_RIT, GrowthCat:="Positive"]
map.scores.by.grade[Spring13_RIT>=ProjectedGrowth, GrowthCat:="Typical"]
map.scores.by.grade[Spring13_RIT>=CollegeReadyGrowth, GrowthCat:="College Ready"]

#Add X in front of students who had negative growth

map.scores.by.grade[GrowthCat=="Negative", StudentFirstLastName:=paste("X",StudentFirstLastName,sep=" ")]

map.scores.by.grade[GrowthCat=="Negative", StudentFirstLastNameRIT:=paste("X",StudentFirstLastNameRIT,sep=" ")]

#Add 'O' in front of students who had negative growth

map.scores.by.grade[GrowthCat=="Positive", StudentFirstLastName:=paste("O",StudentFirstLastName,sep=" ")]

map.scores.by.grade[GrowthCat=="Positive", StudentFirstLastNameRIT:=paste("O",StudentFirstLastNameRIT,sep=" ")]



##********###

# Sort scores scores by grade
map.scores.by.room<-ddply(map.1213, .(Subject, SchoolName, Spring13_Classname), function(df) orderid(df,"Fall12_RIT"))

# Make data.table for easier subsetting and assignment
map.scores.by.room<-data.table(map.scores.by.room)

#Relevel subject factors 
setattr(map.scores.by.room$Subject, "levels", c("Mathematics", "Reading", "Language Usage", "General Science"))

#set growth Categories (note order matters here to get all the categories right)

map.scores.by.room[Spring13_RIT<Fall12_RIT, GrowthCat:="Negative"]
map.scores.by.room[Spring13_RIT>=Fall12_RIT, GrowthCat:="Positive"]
map.scores.by.room[Spring13_RIT>=ProjectedGrowth, GrowthCat:="Typical"]
map.scores.by.room[Spring13_RIT>=CollegeReadyGrowth, GrowthCat:="College Ready"]

#Add X in front of students who had negative growth

map.scores.by.room[GrowthCat=="Negative", StudentFirstLastName:=paste("X",StudentFirstLastName,sep=" ")]

map.scores.by.room[GrowthCat=="Negative", StudentFirstLastNameRIT:=paste("X",StudentFirstLastNameRIT,sep=" ")]

map.summary.1213<-map.1213[,list(pct.meets=sum(Meets)/.N, tot.meets=sum(Meets),  pct.50th.fall=sum(Fall12_Above50th)/.N, tot.50th.fall=sum(Fall12_Above50th), pct.50th.spring=sum(Spring13_Above50th)/.N, tot.50th.spring=sum(Spring13_Above50th), tot.students=.N),by=list(Subject, Fall12_Grade, SchoolInitials)]



cache('map.1213')

cache('map.scores.by.grade')
cache('map.scores.by.room')