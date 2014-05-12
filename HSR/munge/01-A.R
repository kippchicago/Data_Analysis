# HSR preprocessing script.
# each separate school data.table has national and regional data included in it.
# We need to strip out the national and regional data into separate data frames
# and Measure.Names need to be made common across all categories (rather than
# School % Lickert 1, National % Lickert 2, Region % Lickert 4, etc.).

# Strip out regional and national from each data.table

#kap has fewer rows from the middle schools for some reason.  So get regional adn 
#national from KAMS

HSR.Network<-HSR.KAMS[grep("National", Measure.Names)]
HSR.Network[,School:="KIPP Network"]
HSR.Region<-HSR.KAMS[grep("Region", Measure.Names)]
HSR.Region[,School:="KIPP Chicago"]
# remove 
HSR.KAMS<-HSR.KAMS[!(grepl("Region", Measure.Names)|grepl("National", Measure.Names))]

HSR.KAP<-HSR.KAP[!(grepl("Region", Measure.Names)|grepl("National", Measure.Names))]
HSR.KCCP<-HSR.KCCP[!(grepl("Region", Measure.Names)|grepl("National", Measure.Names))]
HSR.KBCP<-HSR.KBCP[!(grepl("Region", Measure.Names)|grepl("National", Measure.Names))]

# combine all 5 into a single data frame
hsr<-rbind(HSR.KAP,HSR.KAMS, HSR.KCCP, HSR.KBCP, HSR.Region, HSR.Network)

# recode all %, #, and other measure names to drop school, regional, region, and average
hsr[,Measure.Names:=gsub("(School|Regional|Region|National)( )(.+)", "\\3", Measure.Names)]


# rename Lickert scales Pct to something usefule and put them in order. 
hsr[Measure.Names=="% Chose Likert 5", Measure.Names:="Strongly Agree"]
hsr[Measure.Names=="% Chose Likert 4", Measure.Names:="Agree"]
hsr[Measure.Names=="% Chose Likert 3", Measure.Names:="Neutral"]
hsr[Measure.Names=="% Chose Likert 2", Measure.Names:="Disagree"]
hsr[Measure.Names=="% Chose Likert 1", Measure.Names:="Strongly Disagree"]

#remove national data from regional top quartile
hsr.regional<-HSR.Regional.Top.Qrtl[!grepl("National", Variable)]

hsr.regional[grepl("ENC", Region), Region:="KIPP ENC"]
hsr.regional[, Region:=factor(as.character(Region), levels=unique(as.character(Region)))]

#remove national data from schools top quartile
hsr.schools<-HSR.Schools.Top.Qrtl[!grepl("National", Variable)]






hsr.quests<-HSR.Regional.questions


hsr.quests[grepl("National", Measure.Names), Level:="National"]
hsr.quests[grepl("Region", Measure.Names), Level:="KIPP Chicago"]

# recode all %, #, and other measure names to drop school, regional, region, and average
hsr.quests[,Measure.Names:=gsub("(School|Regional|Region|National)( )(.+)", "\\3", Measure.Names)]

# rename Lickert scales Pct to something usefule and put them in order. 
hsr.quests[Measure.Names=="% Chose Likert 5", Measure.Names:="Strongly Agree"]
hsr.quests[Measure.Names=="% Chose Likert 4", Measure.Names:="Agree"]
hsr.quests[Measure.Names=="% Chose Likert 3", Measure.Names:="Neutral"]
hsr.quests[Measure.Names=="% Chose Likert 2", Measure.Names:="Disagree"]
hsr.quests[Measure.Names=="% Chose Likert 1", Measure.Names:="Strongly Disagree"]
