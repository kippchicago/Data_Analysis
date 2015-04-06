


drvr <- JDBC("com.mysql.jdbc.Driver","/Users/chaid/Dropbox/JDBC Drivers/mysql-connector-java-5.0.8/mysql-connector-java-5.0.8-bin.jar","")

con <- dbConnect(drvr,"jdbc:mysql://54.245.118.235/db_kippchidata", "chaid", "haiKIPP1" )

sql.query<-"SELECT  t.StudentID AS ID,
t.`StudentFirstName`,
t.`StudentLastName`,
t.`SchoolName`,
t.`Grade` AS Fall12_Grade,	
t.`ClassName`,
t.MeasurementScale AS Subject,
t.GrowthMeasureYN AS Fall12_GM,
t.TestType AS  Fall12_TT, 
t.TestRITScore AS Fall12_RIT,
t.TestPercentile AS Fall12_Pctl,
n.t42 as TypicalFallToSpringGrowth,
n.r42 as ReportedFallToSpringGrowth,
n.s42 as SDFallToSpringGrowth,
t.TestRITScore + n.t42 as NWEAGrowthTarget,
t.TestRITScore + n.t42 + n.s42*0.6744898 as CollegeReadyTarget,
CASE
WHEN TestPercentile >= 75 THEN 4
WHEN TestPercentile < 75 AND TestPercentile>=50 THEN 3
WHEN TestPercentile < 50 AND TestPercentile>=25 THEN 2
ELSE 1
END AS Fall12_Quartile

FROM 	(
  SELECT 	a.*,
  c.ClassName
  FROM `tblClassAssignmentsFall12` as c
  JOIN (
    Select 	r.*, 
    s.DistrictName,
    s.`StudentDateOfBirth`,
    s.`StudentEthnicGroup`,
    s.`StudentLastName`,
    s.`StudentFirstName`,
    s.`StudentMI`,
    s.`Grade`
    FROM	tblAssessmentResultsFall12 as r
    JOIN	tblStudentBySchoolFall12 as s
    ON		r.`StudentID`=s.StudentID
  ) as a
  ON a.StudentID=c.StudentID
) as t
LEFT OUTER JOIN `viewNorms2011_Growth_Kinder_0` as n
ON 		t.`TestRITScore`=n.`StartRIT`
AND		t.`Grade`=n.`StartGrade2`
AND		t.`MeasurementScale`=n.`MeasurementScale`
WHERE GrowthMeasureYN='True' 
AND
(TestType='Survey with Goals'
 OR 
 TestType='Survey'
)
;"

map.F12 <- dbGetQuery(con, sql.query)
map.F12 <- data.table(map.F12)
map.F12 <- map.F12[SchoolName=="KIPP Ascend Middle School"]

map.F12[,GrowthTarget:=round(NWEAGrowthTarget)]
map.F12[,CollegeTarget:=round(CollegeReadyTarget)]

map.reading<-map.F12[Subject=="Reading" & Fall12_GM==TRUE]
# Remove Dellar duplicate.
map.reading<-map.reading[!c(ID==50206087 & ClassName=="Michigan")] 

map.reading[Fall12_Grade==5,GL_Average:=212.4]
map.reading[Fall12_Grade==6,GL_Average:=216.2]
map.reading[Fall12_Grade==7,GL_Average:=219.6]
map.reading[Fall12_Grade==8,GL_Average:=222.6]

l.names<-length(names(map.reading))
setnames(map.reading, names(map.reading[,10:l.names,with=FALSE]), paste(names(map.reading[,10:l.names,with=FALSE]),"Reading",sep="."))



map.math <- map.F12[Subject=="Mathematics"& Fall12_GM==TRUE]
map.math <- map.F12[Subject=="Mathematics"& Fall12_GM==TRUE]
# Remove Dellar duplicate.
map.math<-map.math[!c(ID==50206087 & ClassName=="Michigan")] 

map.math[Fall12_Grade==5,GL_Average:=220.7]
map.math[Fall12_Grade==6,GL_Average:=226.0]
map.math[Fall12_Grade==7,GL_Average:=230.9]
map.math[Fall12_Grade==8,GL_Average:=234.4]


l.names<-length(names(map.math))
setnames(map.math, names(map.math[,10:l.names,with=FALSE]), paste(names(map.math[,10:l.names,with=FALSE]),"Math",sep="."))


setkey(map.reading, ID)
setkey(map.math, ID)
map.mm<-map.reading[map.math]

map.sci[Fall12_Grade==5,GL_Average:=205.3]
map.sci[Fall12_Grade==6,GL_Average:=208.1]
map.sci[Fall12_Grade==7,GL_Average:=210.9]
map.sci[Fall12_Grade==8,GL_Average:=213.5]

setnames(map.sci, names(map.sci[,10:l.names,with=FALSE]), paste(names(map.sci[,10:l.names,with=FALSE]),"Science",sep="."))
setkey(map.sci, ID)

map.mm<-map.sci[map.mm]
write.csv(map.mm, "/Users/chaid/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Data/Output/KAMS_Spring13_Targets_2.csv")



