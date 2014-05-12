# HSR preprocessing script.
# each separate school data.table has national and regional data included in it.
# We need to strip out the national and regional data into separate data frames
# and Measure.Names need to be made common across all categories (rather than
# School % Lickert 1, National % Lickert 2, Region % Lickert 4, etc.).

# Strip out regional and national from each data.table

#kap has fewer rows from the middle schools for some reason.  So get regional adn 
#national from KAMS

HSR.Network.All<-HSR.All[grep("National", Measure.Names)][ 
                           School=="KIPP Ascend Middle School"]
HSR.Network.All[,School:="KIPP Network"]
HSR.Region.All<-HSR.All[grep("Region", Measure.Names)][
                          School=="KIPP Ascend Middle School"]
HSR.Region.All[,School:="KIPP.Chicago"]
# remove 
HSR.Schools.All<-HSR.All[!(grepl("Region", Measure.Names)|grepl("National", Measure.Names))]

# combine all 5 into a single data frame
hsr.all<-rbind(HSR.Schools.All, HSR.Region.All, HSR.Network.All)

# recode all %, #, and other measure names to drop school, regional, region, and average
hsr.all[,Measure.Names:=gsub("(School|Regional|Region|National)( )(.+)", "\\3", Measure.Names)]


# rename Lickert scales Pct to something usefule and put them in order. 
hsr.all[Measure.Names=="% Chose Likert 5", Measure.Names:="Strongly Agree"]
hsr.all[Measure.Names=="% Chose Likert 4", Measure.Names:="Agree"]
hsr.all[Measure.Names=="% Chose Likert 3", Measure.Names:="Neutral"]
hsr.all[Measure.Names=="% Chose Likert 2", Measure.Names:="Disagree"]
hsr.all[Measure.Names=="% Chose Likert 1", Measure.Names:="Strongly Disagree"]

hsr.all[,Measure.Values:=as.numeric(as.character(Measure.Values))]
