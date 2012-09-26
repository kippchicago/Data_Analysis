Fall 2012 MAP Results by Students and Target Setting 
========================================================
This file is the literate programming source for a script that pulls our data from our Data Analysis `MySQL` database. I'll add references to how we get data into the database, as needed.  Nevetheless, this document focuses on the process of how we pull data from the DB and analyse it in [`R`](www.r-project.org)

We need to clean and load out Fall 2012 comprehensive data file (CDF), which we get from [NWEA](http://www.nwea.org)'s reporting website. The CDF is 4 files that contain the students' MAP examination results, student specific information, and classroom data. 

## Prelims
I start by seting the pwd and loading the libraries used in the data analysis.

```r
setwd("~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Code/R")

rm(list = c(ls()))


library(RODBC)  #To get data form Data Analysis DB
library(plyr)  #To manipulate data
library(reshape)  #More data manipulation
```

```
## Attaching package: 'reshape'
```

```
## The following object(s) are masked from 'package:plyr':
## 
## rename, round_any
```

```r
library(ggplot2)  #Graphics of grammer graphing
library(grid)  #More Graphing
```



### Loading Data in MySQL Database
We use two steps to get the data into the database:
  1. *Cleaning*: refactoring the CDF's separate csv files so they comport with `MySQL` conventions. This is done with a "one touch" shell script that usese `sed` and regular expressions to make all necessary character substitions.  
  2. **Loading**:putting the separate CSV files as separate tables into the data analysis database.  Doing so ensures that we maintain the data in as similar manner as we recieve it from NWEA. 

## Retrieving Data from MySQL Database
Once the data loaded, we use the `RODBC` package in `R` to establish a connection ot the databse and run a SQL query to populate a dataframe (NB: `kippchidata2` is an  DSN whith appriate key value pairs that allows an `ODBC` connection to be established with the `MySQL' server:


```r

# Create database connection.  

con<-odbcConnect("kippchidata2")

#get MAP data with SQL statement
map.scores<-sqlQuery(con, 
"SELECT  t.StudentID AS ID,
		t.`StudentFirstName`,
		t.`StudentLastName`,
		t.`Grade`,	
		t.MeasurementScale AS Subject,
		t.GrowthMeasureYN AS Fall12_GM,
		t.TestType AS  Fall12_TT, 
		t.TestRITScore AS Fall12_RIT,
		t.TestPercentile AS Fall12_Pctl,
		n.t42 as TypicalFallToSpringGrowth,
		n.r42 as ReportedFallToSpringGrowth,
		n.s42 as SDFallToSpringGrowth,
		CASE
			WHEN TestPercentile >= 75 THEN 4
			WHEN TestPercentile < 75 AND TestPercentile>=50 THEN 3
			WHEN TestPercentile < 50 AND TestPercentile>=25 THEN 2
			ELSE 1
		END AS Quartile

FROM 	(
		Select 	a.*, 
				s.DistrictName,
				s.`StudentDateOfBirth`,
				s.`StudentEthnicGroup`,
				s.`StudentLastName`,
				s.`StudentFirstName`,
				s.`StudentMI`,
				s.`Grade`
		FROM	tblAssessmentResultsFall12 as a
	 JOIN	tblStudentBySchoolFall12 as s
		ON		a.`StudentID`=s.StudentID
		) as t
LEFT OUTER JOIN `tblNorms2011_Growth` as n
ON 		t.`TestRITScore`=n.`StartRIT`
AND		t.`Grade`=n.`StartGrade`
AND		t.`MeasurementScale`=n.`MeasurementScale`
WHERE 	#GrowthMeasureYN='True' AND
 	(TestType='Survey with Goals'
		OR 
		TestType='Survey'
		)
;
")

#Check contents
head(map.scores)
```

```
##         ID StudentFirstName StudentLastName Grade         Subject
## 1 41921803         Kenyatta          Walker     5 General Science
## 2 41921803         Kenyatta          Walker     5         Reading
## 3 41921803         Kenyatta          Walker     5     Mathematics
## 4 42049832      Christopher           Smith     5 General Science
## 5 42049832      Christopher           Smith     5     Mathematics
## 6 42049832      Christopher           Smith     5         Reading
##   Fall12_GM         Fall12_TT Fall12_RIT Fall12_Pctl
## 1     FALSE Survey With Goals        201          49
## 2     FALSE Survey With Goals        206          47
## 3     FALSE Survey With Goals        206          31
## 4     FALSE Survey With Goals        201          49
## 5     FALSE Survey With Goals        207          34
## 6     FALSE Survey With Goals        219          80
##   TypicalFallToSpringGrowth ReportedFallToSpringGrowth
## 1                      4.03                          4
## 2                      5.26                          5
## 3                      8.10                          8
## 4                      4.03                          4
## 5                      8.11                          8
## 6                      4.62                          5
##   SDFallToSpringGrowth Quartile
## 1                 6.10        2
## 2                 6.13        2
## 3                 5.99        2
## 4                 6.10        2
## 5                 5.99        2
## 6                 6.13        4
```

```r

#Reorder levels (since 13=Kinder) and rename
map.scores$Grade <- factor(map.scores$Grade, levels=c("13", "1", "5", "6","7","8"))
levels(map.scores$Grade) <- c("K", "1", "5", "6","7","8")
```

## 
