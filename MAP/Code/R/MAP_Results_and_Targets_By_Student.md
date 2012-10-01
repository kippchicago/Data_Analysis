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
```

```
##         ID StudentFirstName StudentLastName                SchoolName
## 1 39910470      Chardonne't          Thomas KIPP Ascend Middle School
## 2 39910470      Chardonne't          Thomas KIPP Ascend Middle School
## 3 39984121             Demi          Thomas KIPP Ascend Middle School
## 4 39984121             Demi          Thomas KIPP Ascend Middle School
## 5 40534857          Amarion           Mayes KIPP Ascend Middle School
## 6 40534857          Amarion           Mayes KIPP Ascend Middle School
##   Grade ClassName     Subject Fall12_GM         Fall12_TT Fall12_RIT
## 1     8        ND Mathematics      TRUE Survey With Goals        198
## 2     8        ND     Reading      TRUE Survey With Goals        202
## 3     8        ND Mathematics      TRUE Survey With Goals        221
## 4     8        ND     Reading      TRUE Survey With Goals        215
## 5     8        ND Mathematics      TRUE Survey With Goals        224
## 6     8        ND     Reading      TRUE Survey With Goals        214
##   Fall12_Pctl TypicalFallToSpringGrowth ReportedFallToSpringGrowth
## 1           3                      4.39                          4
## 2          12                      4.26                          4
## 3          29                      4.32                          4
## 4          39                      3.42                          3
## 5          36                      4.31                          4
## 6          36                      3.48                          3
##   SDFallToSpringGrowth Quartile
## 1                 6.42        1
## 2                 6.63        1
## 3                 6.42        2
## 4                 6.63        2
## 5                 6.42        2
## 6                 6.63        2
```

```r

#Reorder levels (since 13=Kinder, prior to Fall 2012, after that it is Kinder=0) and rename
map.scores$Grade <- factor(map.scores$Grade, levels=c("0", "1","2", "5", "6","7","8"))
levels(map.scores$Grade) <- c("K", "1", "2", "5", "6","7","8")
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
##         ID StudentFirstName StudentLastName                SchoolName
## 1 44740729           Machir        Lawrence KIPP Ascend Middle School
## 2 43616412          Antonio          Protho KIPP Ascend Middle School
## 3 43610740           Pierre         Higgins KIPP Ascend Middle School
## 4 91192512           Trevon           Toney KIPP Ascend Middle School
## 5 43571427        Demetrius          Bryant KIPP Ascend Middle School
## 6 44214172          Charles          Rhodes KIPP Ascend Middle School
##   Grade ClassName         Subject Fall12_GM         Fall12_TT Fall12_RIT
## 1     6   Indiana General Science      TRUE Survey With Goals        164
## 2     6   Indiana General Science      TRUE Survey With Goals        171
## 3     6   Indiana General Science      TRUE Survey With Goals        175
## 4     6 Wisconsin General Science      TRUE Survey With Goals        176
## 5     6 Wisconsin General Science      TRUE Survey With Goals        177
## 6     6    Purdue General Science      TRUE Survey With Goals        180
##   Fall12_Pctl TypicalFallToSpringGrowth ReportedFallToSpringGrowth
## 1           1                      5.93                          6
## 2           1                      5.38                          5
## 3           1                      5.07                          5
## 4           1                      4.99                          5
## 5           1                      4.91                          5
## 6           1                      4.68                          5
##   SDFallToSpringGrowth Quartile GrowthPctl75th GrowthTargets
## 1                 5.71        1             10           174
## 2                 5.71        1              9           180
## 3                 5.71        1              9           184
## 4                 5.71        1              9           185
## 5                 5.71        1              9           186
## 6                 5.71        1              9           189
##   StudentLastFirstName StudentFirstLastName OrderID
## 1     Lawrence, Machir      Machir Lawrence       1
## 2      Protho, Antonio       Antonio Protho       2
## 3      Higgins, Pierre       Pierre Higgins       3
## 4        Toney, Trevon         Trevon Toney       4
## 5    Bryant, Demetrius     Demetrius Bryant       5
## 6      Rhodes, Charles       Charles Rhodes       6
```

```r
head(map.scores.by.class)
```

```
##         ID StudentFirstName StudentLastName                SchoolName
## 1 44740729           Machir        Lawrence KIPP Ascend Middle School
## 2 43616412          Antonio          Protho KIPP Ascend Middle School
## 3 43610740           Pierre         Higgins KIPP Ascend Middle School
## 4 91524865            Anika          Stokes KIPP Ascend Middle School
## 5 44707241         Jarvelle       Armstrong KIPP Ascend Middle School
## 6 44725096           Jordan           Greer KIPP Ascend Middle School
##   Grade ClassName         Subject Fall12_GM         Fall12_TT Fall12_RIT
## 1     6   Indiana General Science      TRUE Survey With Goals        164
## 2     6   Indiana General Science      TRUE Survey With Goals        171
## 3     6   Indiana General Science      TRUE Survey With Goals        175
## 4     6   Indiana General Science      TRUE Survey With Goals        185
## 5     6   Indiana General Science      TRUE Survey With Goals        187
## 6     6   Indiana General Science      TRUE Survey With Goals        188
##   Fall12_Pctl TypicalFallToSpringGrowth ReportedFallToSpringGrowth
## 1           1                      5.93                          6
## 2           1                      5.38                          5
## 3           1                      5.07                          5
## 4           3                      4.28                          4
## 5           5                      4.12                          4
## 6           6                      4.05                          4
##   SDFallToSpringGrowth Quartile GrowthPctl75th GrowthTargets
## 1                 5.71        1             10           174
## 2                 5.71        1              9           180
## 3                 5.71        1              9           184
## 4                 5.71        1              8           193
## 5                 5.71        1              8           195
## 6                 5.71        1              8           196
##   StudentLastFirstName StudentFirstLastName OrderID
## 1     Lawrence, Machir      Machir Lawrence       1
## 2      Protho, Antonio       Antonio Protho       2
## 3      Higgins, Pierre       Pierre Higgins       3
## 4        Stokes, Anika         Anika Stokes       4
## 5  Armstrong, Jarvelle   Jarvelle Armstrong       5
## 6        Greer, Jordan         Jordan Greer       6
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
        ggtitle("2012 Fall 1st Grade Reading\nRIT Scores, 
                Expected Growth, and College Ready Growth\nby Quartile")  


###Let's add some summary labels by quaritle to p

#First get the per panel data I want count by quartile, avg y-position (given by OrderID) by quartile,
#  avg RIT by quartile, and percent of quartile students to total studens.

qrtl.labels<-get_group_stats(subset(map.scores.by.grade, Grade==1 & Subject=="Reading"), grp="Quartile")

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
ggsave(p,file="plot_Goal_by_grade_KAPS_1.pdf", path="../../Figures/",height=10.5,width=8)
```


I liked plot so much that I've written a function (`plot_MAP_Results_and_Goals`) so I can very quickly reproduce it for any grade and Class combination, which is sourced above in the `MAP_helper_function.R` script.  Right now it is only useful if the dataframe has very specific column names.  However, it is a stake in the ground that for a later refactoring towards a more general function.  That notwithstanding time pressure, here's the current function in actions


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
```

```
## pdf 
##   2
```

```r

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
```

```
## pdf 
##   2
```

```r



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
```

```
## pdf 
##   2
```

```r


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

```
## pdf 
##   2
```

```r

```


Now for some more high level views compared to the national distribution

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

# KAPS
pmK <- map_comparative_histograms(map_combined_histo_data(kippdata = map.scores.primary, 
    normsdata = nwea.norms.fall, grade = "K", subj = "Mathematics", schoolname = "KAPS"), 
    legendpos = "none", title = "MAP 2012 Kindergarten\nKAPS vs. National\nMath", 
    schoolname = "KAPS")
prK <- map_comparative_histograms(map_combined_histo_data(kippdata = map.scores.primary, 
    normsdata = nwea.norms.fall, grade = "K", subj = "Reading", schoolname = "KAPS"), 
    title = "Reading", schoolname = "KAPS")

pm1 <- map_comparative_histograms(map_combined_histo_data(kippdata = map.scores.primary, 
    normsdata = nwea.norms.fall, grade = 1, subj = "Mathematics", schoolname = "KAPS"), 
    legendpos = "none", title = "MAP 2012 1st Grade\nKAPS vs. National\nMath", 
    schoolname = "KAPS")
pr1 <- map_comparative_histograms(map_combined_histo_data(kippdata = map.scores.primary, 
    normsdata = nwea.norms.fall, grade = 1, subj = "Reading", schoolname = "KAPS"), 
    title = "Reading", schoolname = "KAPS")

pm2 <- map_comparative_histograms(map_combined_histo_data(kippdata = map.scores.primary, 
    normsdata = nwea.norms.fall, grade = 2, subj = "Mathematics", schoolname = "KAPS"), 
    legendpos = "none", title = "MAP 2012 2nd Grade\nKAPS vs. National\nMath", 
    schoolname = "KAPS", schoolname = "KAPS")
```

```
## Error: formal argument "schoolname" matched by multiple actual arguments
```

```r
pr2 <- map_comparative_histograms(map_combined_histo_data(kippdata = map.scores.primary, 
    normsdata = nwea.norms.fall, grade = 2, subj = "Reading", schoolname = "KAPS"), 
    title = "Reading", schoolname = "KAPS")

grid.arrange(pmK, prK, nrow = 2)
```

![plot of chunk plots_histograms](figure/plots_histograms.png) 

```r

pdf(file = "../../Figures/Fall12_MAP_KAPS_Distr.pdf", height = 10.5, width = 8)
grid.arrange(pmK, prK, nrow = 2)
grid.arrange(pm1, pr1, nrow = 2)
grid.arrange(pm2, pr2, nrow = 2)
```

```
## Error: object 'pm2' not found
```

```r

dev.off()
```

```
## pdf 
##   2
```

```r



# KAMS

pm5 <- map_comparative_histograms(map_combined_histo_data(kippdata = map.scores.KAMS, 
    normsdata = nwea.norms.fall, grade = 5, subj = "Mathematics", schoolname = "KAMS"), 
    legendpos = "none", title = "MAP 2012 5th Grade\nKAMS vs. National\nMath")
pr5 <- map_comparative_histograms(map_combined_histo_data(kippdata = map.scores.KAMS, 
    normsdata = nwea.norms.fall, grade = 5, subj = "Reading", schoolname = "KAMS"), 
    title = "Reading")

pm6 <- map_comparative_histograms(map_combined_histo_data(kippdata = map.scores.KAMS, 
    normsdata = nwea.norms.fall, grade = 6, subj = "Mathematics", schoolname = "KAMS"), 
    legendpos = "none", title = "MAP 2012 6th Grade\nKAMS vs. National\nMath")
pr6 <- map_comparative_histograms(map_combined_histo_data(kippdata = map.scores.KAMS, 
    normsdata = nwea.norms.fall, grade = 6, subj = "Reading", schoolname = "KAMS"), 
    title = "Reading")

pm7 <- map_comparative_histograms(map_combined_histo_data(kippdata = map.scores.KAMS, 
    normsdata = nwea.norms.fall, grade = 7, subj = "Mathematics", schoolname = "KAMS"), 
    legendpos = "none", title = "MAP 2012 7th Grade\nKAMS vs. National\nMath")
pr7 <- map_comparative_histograms(map_combined_histo_data(kippdata = map.scores.KAMS, 
    normsdata = nwea.norms.fall, grade = 7, subj = "Reading", schoolname = "KAMS"), 
    title = "Reading")

pm8 <- map_comparative_histograms(map_combined_histo_data(kippdata = map.scores.KAMS, 
    normsdata = nwea.norms.fall, grade = 8, subj = "Mathematics", schoolname = "KAMS"), 
    legendpos = "none", title = "MAP 2012 8th Grade\nKAMS vs. National\nMath")
pr8 <- map_comparative_histograms(map_combined_histo_data(kippdata = map.scores.KAMS, 
    normsdata = nwea.norms.fall, grade = 8, subj = "Reading", schoolname = "KAMS"), 
    title = "Reading")


pdf(file = "../../Figures/Fall12_MAP_KAMS_Distr.pdf", height = 10.5, width = 8)
grid.arrange(pm5, pr5, nrow = 2)
grid.arrange(pm6, pr6, nrow = 2)
grid.arrange(pm7, pr7, nrow = 2)
grid.arrange(pm8, pr8, nrow = 2)
dev.off()
```

```
## pdf 
##   2
```

```r


# KCCP


pm5c <- map_comparative_histograms(map_combined_histo_data(kippdata = map.scores.KCCP, 
    normsdata = nwea.norms.fall, grade = 5, subj = "Mathematics", schoolname = "KCCP"), 
    legendpos = "none", title = "MAP 2012 5th Grade\nKCCP vs. National\nMath", 
    schoolname = "KCCP")
pr5c <- map_comparative_histograms(map_combined_histo_data(kippdata = map.scores.KCCP, 
    normsdata = nwea.norms.fall, grade = 5, subj = "Reading", schoolname = "KCCP"), 
    title = "Reading", schoolname = "KCCP")

pdf(file = "../../Figures/Fall12_MAP_KCCP_Distr.pdf", height = 10.5, width = 8)
grid.arrange(pm5c, pr5c, nrow = 2)
dev.off()
```

```
## pdf 
##   2
```

