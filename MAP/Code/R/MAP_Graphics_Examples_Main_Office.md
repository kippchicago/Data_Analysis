Fall 2012 MAP Results by Students and Target Setting 
========================================================
This file is the literate programming source for a script that pulls our data from our Data Analysis `MySQL` database. I'll add references to how we get data into the database, as needed.  Nevetheless, this document focuses on the process of how we pull data from the DB and analyse it in [`R`](www.r-project.org)

We need to clean and load out Fall 2012 comprehensive data file (CDF), which we get from [NWEA](http://www.nwea.org)'s reporting website. The CDF is 4 files that contain the students' MAP examination results, student specific information, and classroom data. 

## Prelims


I start by setting some global paramaters for my R markdwon file (i.e, this file) by then by seting the present working directory and loading the libraries used in the data analysis.


```r
opts_chunk$set(tidy = TRUE, echo = TRUE, dpi = 150, fig.align = "center", fig.height = 6.3125, 
    fig.width = 5, message = FALSE)
```




```r
setwd("~/Dropbox/Consulting/KIPP Ascend/Data Analysis/MAP/Code/R")



library(RODBC)  #To get data form Data Analysis DB
library(plyr)  #To manipulate data
library(reshape)  #More data manipulation
library(ggplot2)  #Graphics of grammer graphing
library(grid)  #More Graphing
library(gridExtra)  #better than par(mfrow=c(r,c)) for arranging ggplot2 and lattice graphics
library(randomNames)  #to do as it says and generate random names by gender and ethnicity.  Awesome!
```



### Loading Data in MySQL Database
We use two steps to get the data into the database:
  1. **Cleaning**: refactoring the CDF's separate csv files so they comport with `MySQL` conventions. This is done with a "one touch" shell script that usese `sed` and regular expressions to make all necessary character substitions.  
  2. **Loading**:putting the separate CSV files as separate tables into the data analysis database.  Doing so ensures that we maintain the data in as similar manner as we recieve it from NWEA. 

### Retrieving Data from MySQL Database
Once the data loaded, we use the `RODBC` package in `R` to establish a connection ot the databse and run a SQL query to populate a dataframe (NB: `kippchidata2` is an  DSN whith appriate key value pairs that allows an `ODBC` connection to be established with the `MySQL' server.  To protect the privacy of our student's while simultanesouly showning you all my work flow, I've surppressed the evaluation of the following code.  That is, this next chunk of code is not run.


```r

# Create database connection.  

con<-odbcConnect("kippchidata2")

#get MAP data with SQL statement
map.scores<-sqlQuery(con, 
"SELECT  t.StudentID AS ID,
		t.`StudentFirstName`,
		t.`StudentLastName`,
		t.`SchoolName`,
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
LEFT OUTER JOIN `viewNorms2011_Growth_Kinder_0` as n
ON 		t.`TestRITScore`=n.`StartRIT`
AND		t.`Grade`=n.`StartGrade2`
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

#Reorder levels (since 13=Kinder, prior to Fall 2012, after that it is Kinder=0) and rename
map.scores$Grade <- factor(map.scores$Grade, levels=c("0", "1","2", "5", "6","7","8"))
levels(map.scores$Grade) <- c("K", "1", "2", "5", "6","7","8")
```


### Masked Data!!!
Since I didn't run the data loading code that I do run for KIPP Chicago's own reporting, I need to generate some face data, which is pretty straightforward.  Mostly I need fake names to mask the identity of our students.  I'll pull actual test scores from the database so that we can use real targerts and percentiles in the graphcis to follow.


```r

# Create database connection.  
con<-odbcConnect("kippchidata2")

#Pull data from db with SQL query
map.scores<-sqlQuery(con, 
"SELECT  t.`SchoolName`,
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
      AND t.MeasurementScale ='Mathematics'
      AND Grade=5
      AND SchoolName='KIPP Ascend Middle School'                   
;

")

#need to add fake first and last names data using randomName package

#need set of ethnicities to draw form.  95% African American implies 19:1 odds.  Assume gender is 1:1.
ethnicities <- c(rep("African American",19), "Hispanic")
genders <- c("Female", "Male")

#now need to construct 2vectors (length = lenght(map.scores) drawn from the two sets above)

genderethnicity.df<-data.frame(Gender=sample(genders, nrow(map.scores), replace=TRUE), 
                               Ethnicity=sample(ethnicities, nrow(map.scores), replace=TRUE))

names.df<-data.frame(StudentLastName=randomNames(gender=genderethnicity.df$Gender,
                                                 ethnicity=genderethnicity.df$Ethnicity, which.names="last"),
                     StudentFirstName=randomNames(gender=genderethnicity.df$Gender,
                                                  ethnicity=genderethnicity.df$Ethnicity, which.names="first"
                                                  )
                     )

map.scores<-cbind(names.df, map.scores)

head(map.scores)
```

```
##   StudentLastName StudentFirstName                SchoolName Grade
## 1          Nelson           Jordan KIPP Ascend Middle School     5
## 2        Hamilton           Khalil KIPP Ascend Middle School     5
## 3    Ogbaselassie            Caleb KIPP Ascend Middle School     5
## 4        Phillips        Ja'Lainna KIPP Ascend Middle School     5
## 5          Medina         Lakeysia KIPP Ascend Middle School     5
## 6            Cole           La Aja KIPP Ascend Middle School     5
##   ClassName     Subject Fall12_GM         Fall12_TT Fall12_RIT Fall12_Pctl
## 1  Michigan Mathematics      TRUE Survey With Goals        200          18
## 2 Morehouse Mathematics      TRUE Survey With Goals        186           3
## 3      Duke Mathematics      TRUE Survey With Goals        218          64
## 4      Duke Mathematics      TRUE Survey With Goals        199          16
## 5 Morehouse Mathematics      TRUE Survey With Goals        201          20
## 6  Michigan Mathematics      TRUE Survey With Goals        202          22
##   TypicalFallToSpringGrowth ReportedFallToSpringGrowth
## 1                      8.09                          8
## 2                      8.05                          8
## 3                      8.14                          8
## 4                      8.08                          8
## 5                      8.09                          8
## 6                      8.09                          8
##   SDFallToSpringGrowth Quartile
## 1                 5.99        1
## 2                 5.99        1
## 3                 5.99        3
## 4                 5.99        1
## 5                 5.99        1
## 6                 5.99        1
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
map.scores.by.grade <- ddply(map.scores, .(Subject, SchoolName, Grade), function(df) orderid(df, 
    df$Fall12_RIT))
map.scores.by.class <- ddply(map.scores, .(Subject, SchoolName, ClassName), 
    function(df) orderid(df, df$Fall12_RIT))

head(map.scores.by.grade)
```

```
##   StudentLastName StudentFirstName                SchoolName Grade
## 1        Williams           Jabari KIPP Ascend Middle School     5
## 2           Moore           Lexias KIPP Ascend Middle School     5
## 3          Nelson           Patric KIPP Ascend Middle School     5
## 4            Said         Milliona KIPP Ascend Middle School     5
## 5        Hamilton           Khalil KIPP Ascend Middle School     5
## 6            West           Kashon KIPP Ascend Middle School     5
##   ClassName     Subject Fall12_GM         Fall12_TT Fall12_RIT Fall12_Pctl
## 1 Morehouse Mathematics      TRUE Survey With Goals        158           1
## 2      Duke Mathematics      TRUE Survey With Goals        172           1
## 3      Duke Mathematics      TRUE Survey With Goals        172           1
## 4 Morehouse Mathematics      TRUE Survey With Goals        185           2
## 5 Morehouse Mathematics      TRUE Survey With Goals        186           3
## 6      Duke Mathematics      TRUE Survey With Goals        186           3
##   TypicalFallToSpringGrowth ReportedFallToSpringGrowth
## 1                      7.97                          8
## 2                      8.01                          8
## 3                      8.01                          8
## 4                      8.05                          8
## 5                      8.05                          8
## 6                      8.05                          8
##   SDFallToSpringGrowth Quartile GrowthPctl75th GrowthTargets
## 1                 5.99        1             12           170
## 2                 5.99        1             12           184
## 3                 5.99        1             12           184
## 4                 5.99        1             12           197
## 5                 5.99        1             12           198
## 6                 5.99        1             12           198
##   StudentLastFirstName StudentFirstLastName OrderID
## 1     Williams, Jabari      Jabari Williams       1
## 2        Moore, Lexias         Lexias Moore       2
## 3       Nelson, Patric        Patric Nelson       3
## 4       Said, Milliona        Milliona Said       4
## 5     Hamilton, Khalil      Khalil Hamilton       5
## 6         West, Kashon          Kashon West       6
```

```r
head(map.scores.by.class)
```

```
##   StudentLastName StudentFirstName                SchoolName Grade
## 1           Moore           Lexias KIPP Ascend Middle School     5
## 2          Nelson           Patric KIPP Ascend Middle School     5
## 3            West           Kashon KIPP Ascend Middle School     5
## 4      Overstreet           Isaiah KIPP Ascend Middle School     5
## 5           Batey           Brandi KIPP Ascend Middle School     5
## 6           Brown            Sarah KIPP Ascend Middle School     5
##   ClassName     Subject Fall12_GM         Fall12_TT Fall12_RIT Fall12_Pctl
## 1      Duke Mathematics      TRUE Survey With Goals        172           1
## 2      Duke Mathematics      TRUE Survey With Goals        172           1
## 3      Duke Mathematics      TRUE Survey With Goals        186           3
## 4      Duke Mathematics      TRUE Survey With Goals        188           4
## 5      Duke Mathematics      TRUE Survey With Goals        191           6
## 6      Duke Mathematics      TRUE Survey With Goals        193           8
##   TypicalFallToSpringGrowth ReportedFallToSpringGrowth
## 1                      8.01                          8
## 2                      8.01                          8
## 3                      8.05                          8
## 4                      8.05                          8
## 5                      8.06                          8
## 6                      8.07                          8
##   SDFallToSpringGrowth Quartile GrowthPctl75th GrowthTargets
## 1                 5.99        1             12           184
## 2                 5.99        1             12           184
## 3                 5.99        1             12           198
## 4                 5.99        1             12           200
## 5                 5.99        1             12           203
## 6                 5.99        1             12           205
##   StudentLastFirstName StudentFirstLastName OrderID
## 1        Moore, Lexias         Lexias Moore       1
## 2       Nelson, Patric        Patric Nelson       2
## 3         West, Kashon          Kashon West       3
## 4   Overstreet, Isaiah    Isaiah Overstreet       4
## 5        Batey, Brandi         Brandi Batey       5
## 6         Brown, Sarah          Sarah Brown       6
```




```r

#KIPP Foundation approved colors
kippcols<-c("#E27425", "#FEBC11", "#255694", "A7CFEE")

#Plot points for Fall RIT Score, Expected Growth, College Ready Growth, ordered by Fall RIT, Names on Y axis
pointsize<-2
p <- ggplot(map.scores.by.grade, aes(x=Fall12_RIT, y=OrderID)) +
    geom_text(aes(x=Fall12_RIT-1, 
                  color=as.factor(Quartile), 
                  label=StudentFirstLastName), 
              size=2, 
              hjust=1) +
    geom_point(aes(color=as.factor(Quartile)), size=pointsize) +
    geom_text(aes(x=Fall12_RIT+1, color=as.factor(Quartile), label=Fall12_RIT), size=2, hjust=0) +
    geom_point(aes(x=Fall12_RIT + ReportedFallToSpringGrowth, y=OrderID), color="#CFCCC1", size=pointsize) +
    geom_text(aes(x=Fall12_RIT + ReportedFallToSpringGrowth+1, 
                  label=Fall12_RIT + ReportedFallToSpringGrowth), 
              color="#CFCCC1", 
              size=2, 
              hjust=0) +
    geom_point(aes(x=GrowthTargets, y=OrderID), color="#FEBC11", size=pointsize) + 
    geom_text(aes(x=GrowthTargets+1, label=GrowthTargets), color="#FEBC11", size=2, hjust=0) +
    facet_grid(Quartile~., scale="free_y", space = "free_y", as.table=FALSE) +
    scale_colour_discrete(kippcols) + 
    scale_y_continuous(" ", breaks=map.scores.by.grade$OrderID, expand=c(0,1)) + 
    theme(axis.text.y = element_text(size=3, hjust=1)) + 
    theme(legend.position = "none") + 
    scale_x_continuous("RIT Score") + 
    expand_limits(x=115)+
      theme(
        panel.background = element_rect(fill = "transparent",colour = NA), # or element_blank()
        # panel.grid.minor = element_blank(), 
        # panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text.x = element_text(size=15),
        axis.text.y = element_blank(), 
        #axis.title.y = element_blank(), 
        axis.ticks=element_blank(),
    
        strip.text.x=element_text(size=15),
        strip.text.y=element_text(size=15,angle=0), 
        strip.background=element_rect(fill="#F4EFEB", colour=NA),
        plot.title=element_text(size=12)
        ) +
        ggtitle("2012 Fall 5th Grade Mathematics\nRIT Scores, 
                Expected Growth, and College Ready Growth\nby Quartile")  


###Let's add some summary labels by quaritle to p

#First get the per panel data I want count by quartile, avg y-position (given by OrderID) by quartile,
#  avg RIT by quartile, and percent of quartile students to total studens.

qrtl.labels<-get_group_stats(map.scores.by.grade, grp="Quartile")

#add a column with the actual label text
qrtl.labels$CountLabel<-paste(qrtl.labels$CountStudents," students (",round(qrtl.labels$PctofTotal*100),"%)", sep="")

qrtl.labels$AvgLabel<-paste("Avg RIT = ",round(qrtl.labels$AvgQrtlRIT))

#eyeballed X position
qrtl.labels$xpos<-rep(150,nrow(qrtl.labels))

#now adding this info to the plot p
p <- p + geom_text(data=qrtl.labels, aes(x=xpos, y=AvgCountID, color=factor(Quartile),label=CountLabel),vjust=0, size=3.25) +
    geom_text(data=qrtl.labels, aes(x=xpos, y=AvgCountID, color=factor(Quartile),label=AvgLabel),vjust=1.5, size=3.25)

p
```

![plot of chunk plot_Goal_by_grade](figure/plot_Goal_by_grade.png) 

```r

#Uncomment belose to Save pdf vector file of plot for other uses. 
#ggsave(p,file="plot_Goal_by_grade_KAPS_1.pdf", path="../../Figures/",height=10.5,width=8)
```


I liked plot so much that I've written a function (`plot_MAP_Results_and_Goals`) so I can very quickly reproduce it for any grade and Class combination, which is sourced above in the `MAP_helper_function.R` script.  Right now it is only useful if the dataframe has very specific column names.  However, it is a stake in the ground that for a later refactoring towards a more general function.  That notwithstanding time pressure, here's the current function in actions (**NB: This code isn't evaluated here; it is only an exmaple **)


```r
# Relevel subject factors
map.scores.by.grade$Subject <- factor(map.scores.by.grade$Subject, levels = c("Mathematics", 
    "Reading", "Language Usage", "General Science"))

map.scores.by.class$Subject <- factor(map.scores.by.grade$Subject, levels = c("Mathematics", 
    "Reading", "Language Usage", "General Science"))

### Separate PDF for each School


# KAPS First by Grade
map.scores.primary <- subset(map.scores.by.grade, SchoolName == "KIPP Ascend Primary")

pdf(file = "../../Figures/Fall12_MAP_KAPS.pdf", height = 10.5, width = 8)

for (s in sort(unique(map.scores.primary$Subject))) {
    dfp <- subset(map.scores.primary, Subject == s)  #DataFrame to Plot
    for (g in as.character(sort(unique(dfp$Grade)))) {
        ptitle <- paste("KAPS 2012 Fall MAP Grade ", g, " ", s, "\nRIT Scores, Expected Growth, and College Ready Growth\nby Quartile", 
            sep = "")
        p <- plot_MAP_Results_and_Goals(subset(dfp, Grade == g), ptitle, labxpos = 113, 
            minx = 104)
        print(p)
    }
}
dev.off()

# Then by Classroom (KAPS only)

map.scores.primary.by.class <- subset(map.scores.by.class, SchoolName == "KIPP Ascend Primary" & 
    ID != "50206087")
# This kid is assinged to two classes in math as a 1st grader but the
# classes are K classees. Weird.

pdf(file = "../../Figures/Fall12_MAP_KAPS_by_Classroom.pdf", height = 10.5, 
    width = 8)
# Need to Loop by Subject, then grade, then classroom
for (s in as.character(sort(unique(map.scores.primary.by.class$Subject)))) {
    dfs <- subset(map.scores.primary.by.class, Subject == s)  #DataFrame for subject
    for (g in as.character(sort(unique(dfs$Grade)))) {
        dfp <- subset(dfs, Grade == g)  #DataFrame for Plot
        for (c in as.character(sort(unique(dfp$ClassName)))) {
            ptitle <- paste("KAPS 2012 Fall MAP ", c, " (", g, ") ", s, "\nRIT Scores, Expected Growth, and College Ready Growth\nby Quartile", 
                sep = "")
            p <- plot_MAP_Results_and_Goals(subset(dfp, ClassName == c), ptitle, 
                labxpos = 113, minx = 104)
            print(p)
        }
    }
}
dev.off()



# KAMS
map.scores.KAMS <- subset(map.scores.by.grade, SchoolName == "KIPP Ascend Middle School")

pdf(file = "../../Figures/Fall12_MAP_KAMS.pdf", height = 10.5, width = 8)

for (s in sort(unique(map.scores.KAMS$Subject))) {
    dfp <- subset(map.scores.KAMS, Subject == s)  #DataFrame to Plot
    for (g in as.character(sort(unique(dfp$Grade)))) {
        ptitle <- paste("KAMS 2012 Fall MAP Grade ", g, " ", s, "\nRIT Scores, Expected Growth, and College Ready Growth\nby Quartile", 
            sep = "")
        p <- plot_MAP_Results_and_Goals(subset(dfp, Grade == g), ptitle, labxpos = 170, 
            minx = 145, alp = 0.6)
        print(p)
    }
}
dev.off()


# KCCP
map.scores.KCCP <- subset(map.scores.by.grade, SchoolName == "KIPP Create Middle School")

pdf(file = "../../Figures/Fall12_MAP_KCCP.pdf", height = 10.5, width = 8)

for (s in sort(unique(map.scores.KCCP$Subject))) {
    dfp <- subset(map.scores.KCCP, Subject == s)  #DataFrame to Plot
    for (g in as.character(sort(unique(dfp$Grade)))) {
        ptitle <- paste("KCCP 2012 Fall MAP Grade ", g, " ", s, "\nRIT Scores, Expected Growth, and College Ready Growth\nby Quartile", 
            sep = "")
        p <- plot_MAP_Results_and_Goals(subset(dfp, Grade == g), ptitle, labxpos = 150, 
            minx = 140, alp = 0.6)
        print(p)
    }
}
dev.off()

```


Now for some more high level views compared to the national distribution.  These figures are helpful in understanding where a whole grade or classroom is relative to nationally representative distribution.  However, we don't have such a distribution, so I simulated one using the nationally normed means and standard deviations for each subject-grade pair.  I assumed that the distributions were truncated Guasian (i.e., normal) distriubtions and used some basic probabity theory to construct the a smaple distribution.  The code for this is in the `MAP_helper_fucntions.R` script in the `map_combined_histo_data()` function.  It's worth a look if you want to see how almost any distribution can be built up by starting with the $U\sim(0,1)$, i.e., the uniform distribution over the interval from 0 to 1.  The historgrams themselves are generated with the `map_comparative_histograms()` function in the same file

```r
# get national summary statistics for Reading and Math, Grades K-2,5-8 for
# simulation
nwea.norms.fall <- data.frame(Grade = factor(c("K", "K", "1", "1", "2", "2", 
    "5", "5", "6", "6", "7", "7", "8", "8"), levels = c("K", "1", "2", "5", 
    "6", "7", "8")), Subject = factor(c("Mathematics", "Reading", "Mathematics", 
    "Reading", "Mathematics", "Reading", "Mathematics", "Reading", "Mathematics", 
    "Reading", "Mathematics", "Reading", "Mathematics", "Reading"), levels = c("Mathematics", 
    "Reading")), Mean = c(143.7, 142.5, 162.8, 160.3, 178.2, 175.9, 212.9, 209.8, 
    219.6, 212.3, 225.6, 216.3, 230.2, 219.3), SD = c(11.88, 10.71, 13.57, 12.76, 
    12.97, 15.44, 14.18, 14.21, 15.37, 14.39, 16.79, 14.23, 17.04, 14.86))





# KAMS

pm5 <- map_comparative_histograms(map_combined_histo_data(kippdata = map.scores.by.grade, 
    normsdata = nwea.norms.fall, grade = 5, subj = "Mathematics", schoolname = "KAMS"), 
    legendpos = "none", title = "MAP 2012 5th Grade\nKAMS vs. National\nMath")

pm5
```

![plot of chunk plots_histograms](figure/plots_histograms.png) 

