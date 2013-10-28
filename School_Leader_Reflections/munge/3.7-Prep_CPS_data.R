# Combine CPS MAP Data with Network MAP data

map.cps<-copy(MAP.CPS.Network.1213)


#Drop extraneous columns
map.cps[,c(
              "Average_RIT_Spring2013",
              "Pct_At_or_Above_Natl_Avg_Spring2013",
              "CPSID",
              "CPS_Network",
              "Pct_At_or_Above_Natl_Avg_Growth_Score",
              "Average_RIT_Fall2012",
              "SY"):=NULL]

#map.cps[,PCt_50_Pctl:=NULL]

# Recast factors to numerics
map.cps[,AGG_Pct_At_or_Above_Natl_Avg_Growth_Score:=as.character(AGG_Pct_At_or_Above_Natl_Avg_Growth_Score)]

map.cps[,AGG_Pct_At_or_Above_Natl_Avg_Growth_Score:=as.numeric(str_extract(AGG_Pct_At_or_Above_Natl_Avg_Growth_Score, "[[:digit:]]+"))]




setnames(map.cps, names(map.cps), c("End_RIT",
                                    "Perc_Above_50th",
                                    "Perc_Growth",
                                    "Start_RIT",
                                    "Growth_Grade_Level",
                                    "N", 
                                    "Sub_Test_Name",
                                    "Region_Name", 
                                    "School_Display_Name",
                                    "TotalTested_Fall12",
                                    "TotalTested_Spring13"
                                       )
            )


map.cps[,c("TotalTested_Fall12",
  "TotalTested_Spring13"):=NULL]

map.cps[,Growth.Season:="Fall to Spring"]

map.cps[,School_Display_Name_Chi:="CPS"]
map.cps[,Growth_Grade_Level:=as.numeric(str_extract(Growth_Grade_Level,"[[:digit:]]"))]


### All Grades combined ####
map.combined.grades<-copy(MAP.CPS.Network.1213[Grade=="All Grades Combined"])

#Drop extraneous columns
map.combined.grades[,c(
  "Average_RIT_Spring2013",
  "Pct_At_or_Above_Natl_Avg_Spring2013",
  "CPSID",
  "CPS_Network",
  "Pct_At_or_Above_Natl_Avg_Growth_Score",
  "Average_RIT_Fall2012",
  "SY"):=NULL]

#map.combined.grades[,PCt_50_Pctl:=NULL]

# Recast factors to numerics
map.combined.grades[,AGG_Pct_At_or_Above_Natl_Avg_Growth_Score:=as.character(AGG_Pct_At_or_Above_Natl_Avg_Growth_Score)]

map.combined.grades[,AGG_Pct_At_or_Above_Natl_Avg_Growth_Score:=as.numeric(str_extract(AGG_Pct_At_or_Above_Natl_Avg_Growth_Score, "[[:digit:]]+"))]




setnames(map.combined.grades, names(map.combined.grades), c("End_RIT",
                                    "Perc_Above_50th",
                                    "Perc_Growth",
                                    "Start_RIT",
                                    "Growth_Grade_Level",
                                    "N", 
                                    "Sub_Test_Name",
                                    "Region_Name", 
                                    "School_Display_Name",
                                    "TotalTested_Fall12",
                                    "TotalTested_Spring13"
)
)


map.combined.grades[,c("TotalTested_Fall12",
           "TotalTested_Spring13"):=NULL]

map.combined.grades[,Growth.Season:="Fall to Spring"]

map.combined.grades[,School_Display_Name_Chi:="CPS"]
