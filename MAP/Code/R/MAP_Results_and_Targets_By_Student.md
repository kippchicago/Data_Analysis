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
library(directlables)  #direct labeling in ggplot2 and lattice plots
```

```
## Error: there is no package called 'directlables'
```



### Loading Data in MySQL Database
We use two steps to get the data into the database:
  1. **Cleaning**: refactoring the CDF's separate csv files so they comport with `MySQL` conventions. This is done with a "one touch" shell script that usese `sed` and regular expressions to make all necessary character substitions.  
  2. **Loading**:putting the separate CSV files as separate tables into the data analysis database.  Doing so ensures that we maintain the data in as similar manner as we recieve it from NWEA. 

### Retrieving Data from MySQL Database
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
		t.`ClassName`,
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
##         ID StudentFirstName StudentLastName Grade  ClassName
## 1 41921803         Kenyatta          Walker     5 Pittsburgh
## 2 41921803         Kenyatta          Walker     5 Pittsburgh
## 3 41921803         Kenyatta          Walker     5 Pittsburgh
## 4 42049832      Christopher           Smith     5   Grinnell
## 5 42049832      Christopher           Smith     5   Grinnell
## 6 42049832      Christopher           Smith     5   Grinnell
##           Subject Fall12_GM         Fall12_TT Fall12_RIT Fall12_Pctl
## 1 General Science     FALSE Survey With Goals        201          49
## 2         Reading     FALSE Survey With Goals        206          47
## 3     Mathematics     FALSE Survey With Goals        206          31
## 4 General Science     FALSE Survey With Goals        201          49
## 5     Mathematics     FALSE Survey With Goals        207          34
## 6         Reading     FALSE Survey With Goals        219          80
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

#Reorder levels (since 13=Kinder, prior to Fall 2012, after that it is Kinder=0) and rename
map.scores$Grade <- factor(map.scores$Grade, levels=c("0", "1", "5", "6","7","8"))
levels(map.scores$Grade) <- c("K", "1", "5", "6","7","8")
```

## MAP Target Setting

The term "Growth Target" is misnomer.  The "growth target" is simply a students expected (or average) growth contional on thier starting RIT score and current grade.  That is, the perior 1 to period 2  "growth target" is the average differnce in period 1 and period 2 scores for all students in a particular grade with same period 1 score.  For this reason I refer to the the NWEA supplied "growth targets" as *expected growth* (a statitically meaningful term) or *typical growth* (a substantively meaningful term). As Andrew Martin at Team schools, among others, has made clear, if our students meerly hit their expected growht numbers every year through 11th grade they will on average not be "Rutgers Ready" (Team's clever alliteration).

Time requires that I eschew Andrew's quadratic fit goal setting that they are employing in Newark. Instead, I use the data we have from the 2011 MAP Norms Table give provide our teachers and students targets that at the 75th percentile of growth (i.e., the mean plus .675 standard deviations). 


```r
# get z score (i.e., number of standard deviations) that corresponds to
# 75th percentile
sigma <- qnorm(0.75)
# add simga*SD to mean and round to integer
map.scores$GrowthPctl75th <- round(map.scores$TypicalFallToSpringGrowth + sigma * 
    map.scores$SDFallToSpringGrowth, 0)

# calculate targets
map.scores$GrowthTargets <- map.scores$Fall12_RIT + map.scores$GrowthPctl75th

# Combine Student First and Last Names into one field

map.scores$StudentLastFirstName <- paste(map.scores$StudentLastName, map.scores$StudentFirstName, 
    sep = ", ")
map.scores$StudentFirstLastName <- paste(map.scores$StudentFirstName, map.scores$StudentLastName, 
    sep = " ")
```


## Figures
So now we move on to graphing, leaning heavily (completely?) on the Hadley Wickham's `ggplot` package.

In order to list students by order of test scores, I need a function ot add a column that adds a counter after they are sorted on a given column's values.  Fortuitously, I've written this function---along with a number of other helper functions for this analysis---in an `R` script called (perhaps obvoiusly) `MAP_helper_functions.R`, which is located in this directory of the GIT repo.

```r
source("MAP_helper_functions.R")
```

OK. Now to graphics.  Here I want to graph the fall score, the expected growth and the college ready 75th percentile growth.  Since we want graphs by grade we need to use `ddply` to run `fn_orderid` over each grade as well as each classroom for each subject:


```r
map.scores.by.grade <- ddply(map.scores, .(Subject, Grade), function(df) orderid(df, 
    df$Fall12_RIT))
map.scores.by.class <- ddply(map.scores, .(Subject, ClassName), function(df) orderid(df, 
    df$Fall12_RIT))

head(map.scores.by.grade)
```

```
##         ID StudentFirstName StudentLastName Grade  ClassName
## 1 43712128           Jamiya          Foster     5 Pittsburgh
## 2 44181010          Deshaun         Chapman     5 Pittsburgh
## 3 91715210           Keenan           White     5   Grinnell
## 4 45372995          Mikayla            Ware     5   Grinnell
## 5 44763559           Kaylin          Ruffin     5   Grinnell
## 6 44771365          Quavinn          Ingram     5      Texas
##           Subject Fall12_GM         Fall12_TT Fall12_RIT Fall12_Pctl
## 1 General Science     FALSE Survey With Goals        171           1
## 2 General Science     FALSE Survey With Goals        175           1
## 3 General Science     FALSE Survey With Goals        176           1
## 4 General Science     FALSE Survey With Goals        180           2
## 5 General Science     FALSE Survey With Goals        181           3
## 6 General Science     FALSE Survey With Goals        182           4
##   TypicalFallToSpringGrowth ReportedFallToSpringGrowth
## 1                      5.73                          6
## 2                      5.50                          6
## 3                      5.45                          5
## 4                      5.22                          5
## 5                      5.16                          5
## 6                      5.11                          5
##   SDFallToSpringGrowth Quartile GrowthPctl75th GrowthTargets
## 1                  6.1        1             10           181
## 2                  6.1        1             10           185
## 3                  6.1        1             10           186
## 4                  6.1        1              9           189
## 5                  6.1        1              9           190
## 6                  6.1        1              9           191
##   StudentLastFirstName StudentFirstLastName OrderID
## 1       Foster, Jamiya        Jamiya Foster       1
## 2     Chapman, Deshaun      Deshaun Chapman       2
## 3        White, Keenan         Keenan White       3
## 4        Ware, Mikayla         Mikayla Ware       4
## 5       Ruffin, Kaylin        Kaylin Ruffin       5
## 6      Ingram, Quavinn       Quavinn Ingram       6
```

```r
head(map.scores.by.class)
```

```
##         ID StudentFirstName StudentLastName Grade ClassName
## 1 91715210           Keenan           White     5  Grinnell
## 2 45372995          Mikayla            Ware     5  Grinnell
## 3 44763559           Kaylin          Ruffin     5  Grinnell
## 4 50053590           Jawonn             Nix     5  Grinnell
## 5 44296373          Takayla          Walker     5  Grinnell
## 6 50078086          Brandon           White     5  Grinnell
##           Subject Fall12_GM         Fall12_TT Fall12_RIT Fall12_Pctl
## 1 General Science     FALSE Survey With Goals        176           1
## 2 General Science     FALSE Survey With Goals        180           2
## 3 General Science     FALSE Survey With Goals        181           3
## 4 General Science     FALSE Survey With Goals        186           8
## 5 General Science     FALSE Survey With Goals        188          11
## 6 General Science     FALSE Survey With Goals        188          11
##   TypicalFallToSpringGrowth ReportedFallToSpringGrowth
## 1                      5.45                          5
## 2                      5.22                          5
## 3                      5.16                          5
## 4                      4.88                          5
## 5                      4.77                          5
## 6                      4.77                          5
##   SDFallToSpringGrowth Quartile GrowthPctl75th GrowthTargets
## 1                  6.1        1             10           186
## 2                  6.1        1              9           189
## 3                  6.1        1              9           190
## 4                  6.1        1              9           195
## 5                  6.1        1              9           197
## 6                  6.1        1              9           197
##   StudentLastFirstName StudentFirstLastName OrderID
## 1        White, Keenan         Keenan White       1
## 2        Ware, Mikayla         Mikayla Ware       2
## 3       Ruffin, Kaylin        Kaylin Ruffin       3
## 4          Nix, Jawonn           Jawonn Nix       4
## 5      Walker, Takayla       Takayla Walker       5
## 6       White, Brandon        Brandon White       6
```




```r

#KIPP Foundation approved colors
kippcols<-c("#E27425", "#FEBC11", "#255694", "A7CFEE")

#Plot points for Fall RIT Score, Expected Growth, College Ready Growth, ordered by Fall RIT, Names on Y axis
pointsize<-2
p <- ggplot(subset(map.scores.by.grade, Grade==1 & Subject=="Reading"), aes(x=Fall12_RIT, y=OrderID)) +
    geom_text(aes(x=Fall12_RIT-1, color=as.factor(Quartile), label=StudentFirstLastName), size=2, hjust=1) +
    geom_point(aes(color=as.factor(Quartile)), size=pointsize) +
    geom_text(aes(x=Fall12_RIT+1, color=as.factor(Quartile), label=Fall12_RIT), size=2, hjust=0) +
    geom_point(aes(x=Fall12_RIT + ReportedFallToSpringGrowth, y=OrderID), color="#CFCCC1", size=pointsize) +
    geom_text(aes(x=Fall12_RIT + ReportedFallToSpringGrowth+1, label=Fall12_RIT + ReportedFallToSpringGrowth), color="#CFCCC1", size=2, hjust=0) +
    geom_point(aes(x=GrowthTargets, y=OrderID), color="#FEBC11", size=pointsize) + 
    geom_text(aes(x=GrowthTargets+1, label=GrowthTargets), color="#FEBC11", size=2, hjust=0) +
    facet_grid(Quartile~., scale="free_y", space = "free_y", as.table=FALSE) +
    scale_colour_discrete(kippcols) + 
    scale_y_continuous(" ", breaks=map.scores.by.grade$OrderID, expand=c(0,1)) + 
    opts(axis.text.y = theme_text(size=3, hjust=1)) + 
    opts(legend.position = "none") + 
    scale_x_continuous("RIT Score") + 
    expand_limits(x=115)+
      opts(
        panel.background = theme_rect(fill = "transparent",colour = NA), # or theme_blank()
        # panel.grid.minor = theme_blank(), 
        # panel.grid.major = theme_blank(),
        plot.background = theme_rect(fill = "transparent",colour = NA),
        axis.text.x = theme_text(size=15),
        axis.text.y = theme_blank(), 
        #axis.title.y = theme_blank(), 
        axis.ticks=theme_blank(),
    
        strip.text.x=theme_text(size=15),
        strip.text.y=theme_text(size=15,angle=0), 
        strip.background=theme_rect(fill="#F4EFEB", colour=NA),
        title="2012 Fall 1st Grade Reading\nRIT Scores, Expected Growth, and College Ready Growth\nby Quartile",
        plot.title=theme_text(size=12)
        ) 


###Let's add some summary labels by quaritle to p

#First get the per panel data I want count by quartile, avg y-position (given by OrderID) by quartile,
#  avg RIT by quartile, and percent of quartile students to total studens.

qrtl.labels<-get_group_stats(subset(map.scores.by.grade, Grade==1), grp="Quartile")

#add a column with the actual label text
qrtl.labels$CountLabel<-paste(qrtl.labels$CountStudents," students (",round(qrtl.labels$PctofTotal*100),"%)", sep="")

qrtl.labels$AvgLabel<-paste("Avg RIT = ",round(qrtl.labels$AvgQrtlRIT))

#eyeballed X position
qrtl.labels$xpos<-rep(120,nrow(qrtl.labels))

#now adding this info to the plot p
p <- p + geom_text(data=qrtl.labels, aes(x=xpos, y=AvgCountID, color=factor(Quartile),label=CountLabel),vjust=0, size=3.25) +
    geom_text(data=qrtl.labels, aes(x=xpos, y=AvgCountID, color=factor(Quartile),label=AvgLabel),vjust=1.5, size=3.25)

p
```

![plot of chunk plot_Goal_by_grade](figure/plot_Goal_by_grade.png) 

```r

#Save pdf vector file of plot for other uses. 
ggsave(p,file="plot_Goal_by_grade.pdf", path="../../Figures/",height=10.5,width=8)

```


