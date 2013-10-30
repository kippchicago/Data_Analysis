# Preporcessing Script for MAP Analysis


# Prep data for Season to Season Arrow Charts

# Sort scores scores by grade
map.scores.by.grade<-ddply(map.S13F13, .(Subject, SchoolName, Fall13_Grade), function(df) orderid(df,"Fall13_RIT"))

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



cache('anet.math.1112')
cache('anet.read.1112')
cache('anet.math.1213')
cache('anet.read.1213')
cache('map.1112')
cache('map.1112.ss')
cache('map.1213')
cache('map.1213.ss')
cache('map.scores.by.grade')
cache('map.scores.by.room')