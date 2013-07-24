
# Reduce ISAT panel to only M/E for reading and math.  Drop combined grades
# results as well
ISAT.kams.plotdata<-copy(ISAT.panel.Ascend[,list(Grade_Level,School_Year,  ISAT_Read_MeetExceed_Percent, ISAT_Math_MeetExceed_Percent, ISAT_Composite_MeetExceed_Percent)])

# create better/shorter grade level column
ISAT.kams.plotdata[,Grade:=(str_extract(Grade_Level,"\\d"))]
ISAT.kams.plotdata[Grade=="3",Grade:="5-8"]


# Melt Data
ISAT.kams.plotdata<-data.table(melt(ISAT.kams.plotdata, 
                                    id.vars=c("Grade", 
                                              "Grade_Level", 
                                              "School_Year"
                                              )
                                    )
                               )

# Fix varialbe names to subjects
ISAT.kams.plotdata[,variable:=str_extract(
                                    str_extract(variable, "_([[:alpha:]]+)_"),
                                    "[[:alpha:]]+"
                                    )]

# Fix School Year to numeric
ISAT.kams.plotdata[,School_Year:=
                gsub("(\\d+ - )(\\d+)", 
                     "\\2", 
                     School_Year
                     )
              ]

#drop grade level
ISAT.kams.plotdata[,Grade_Level:=NULL]
setnames(ISAT.kams.plotdata, "School_Year", "Year")
ISAT.kams.plotdata[,School:="KAMS"]

# TO DO: get KCCP

ISAT.kccp.plotdata<-copy(ISAT.panel.KCCP[,list(Grade_Level,School_Year,  ISAT_Read_MeetExceed_Percent, ISAT_Math_MeetExceed_Percent, ISAT_Composite_MeetExceed_Percent)])

# create better/shorter grade level column
ISAT.kccp.plotdata[,Grade:=(str_extract(Grade_Level,"\\d"))]
ISAT.kccp.plotdata[Grade=="3",Grade:="5-8"]


# Melt Data
ISAT.kccp.plotdata<-data.table(melt(ISAT.kccp.plotdata, 
                                    id.vars=c("Grade", 
                                              "Grade_Level", 
                                              "School_Year"
                                              )
                                  )
                                )

# Fix varialbe names to subjects
ISAT.kccp.plotdata[,variable:=str_extract(
  str_extract(variable, "_([[:alpha:]]+)_"),
  "[[:alpha:]]+"
)]

# Fix School Year to numeric
ISAT.kccp.plotdata[,School_Year:=
                     gsub("(\\d+ - )(\\d+)", 
                          "\\2", 
                          School_Year
                     )
                   ]

#drop grade level
ISAT.kccp.plotdata[,Grade_Level:=NULL]
setnames(ISAT.kccp.plotdata, "School_Year", "Year")
ISAT.kccp.plotdata[,School:="KCCP"]


####################
####################
###################


# get CPS Data (notice that variable names are slightly different)
ISAT.cps.plotdata<-copy(ISAT.panel.CPS[!Grade %in% 
                                         c("3rd Grade","4th Grade"),
                                       list(Grade, 
                                            Year,  
                                            ISAT_Read_MeetExceed_Percent,
                                            ISAT_Math_MeetExceed_Percent,
                                            ISAT_Composite_MeetExceed_Percent)
                                       ]
                        )


ISAT.cps.plotdata<-data.table(melt(ISAT.cps.plotdata, id.vars=c("Year", "Grade")))

#fix variable names
ISAT.cps.plotdata[,variable:=str_extract(
  str_extract(variable, "_([[:alpha:]]+)_"),
  "[[:alpha:]]+"
)
              ]


# create better/shorter grade level column
ISAT.cps.plotdata[,Grade:=str_extract(Grade,"\\d")]
ISAT.cps.plotdata[Grade=="3", Grade:="5-8"]

ISAT.cps.plotdata[,School:="CPS"]


#combine KAMS, KCCP, and CPS data 
ISAT.plotdata<-rbind(ISAT.kams.plotdata,ISAT.kccp.plotdata, ISAT.cps.plotdata, use.names=TRUE)

#reorder grades
ISAT.plotdata[,Grade:=factor(Grade)]
ISAT.plotdata[,Grade:=factor(Grade, levels=c("8","7","6","5","5-8"))]

#add y label position for table
ISAT.plotdata[School=="CPS",y.label.pos:=0.25]
ISAT.plotdata[School=="KAMS",y.label.pos:=-0.25]
ISAT.plotdata[School=="KCCP",y.label.pos:=0]


#max and mins for KAMS over time
ISAT.maxmin<-rbind(ISAT.kams.plotdata[Year>=2007,list(value=min(value), grp="min"), by=list(variable, Grade)],ISAT.kams.plotdata[Year>=2007,list(value=max(value), grp="max"), by=list(variable, Grade)])

setkey(ISAT.maxmin, variable, Grade, value)
setkey(ISAT.kams.plotdata, variable, Grade, value)

ISAT.maxmin<-ISAT.kams.plotdata[ISAT.maxmin]



