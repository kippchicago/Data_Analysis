#### Transfers #### 

#table of Transfers by schoool 


Xfersreasons<-data.table(EXITCODE=as.character(c(1:11,99)),Reason=c("Dropped Out", 
                                                                    "Moved", 
                                                                    "Transport", 
                                                                    "Expelled", 
                                                                    "Behavior/Discipline", 
                                                                    "Academics", 
                                                                    "Avoid Retention", 
                                                                    "Special Needs", 
                                                                    "Other", 
                                                                    "Don't Know", 
                                                                    "Xfer Other KIPP", 
                                                                    "DNA"))

setkey(Xfers.HSR.1314, EXITCODE)
setkey(Xfersreasons, EXITCODE)


Xfer.students.table<-copy(Xfersreasons[Xfers.HSR.1314])

# Create summary table
Xfer.table<-cast(ddply(Xfer.students.table, .(Reason, SCHOOLID), function(df)c(N=nrow(df))), Reason~SCHOOLID, margins=TRUE, fun.aggregate=sum)
names(Xfer.table)<-c("Transfer Reason", "KAMS", "KAP", "KCCP", "KBCP", "Region")
levels(Xfer.table[,1])[length(Xfer.table[,1])]<-"Total"
Xfer.table<-Xfer.table[,c("Transfer Reason", "KAP", "KAMS", "KCCP", "KBCP", "Region")]

# Create table of each student transferred with School, Grade, reason, comment.
Xfer.students.table[, LastFirst:= paste(LAST_NAME, FIRST_NAME, sep=', ')]
Xfer.students.table[SCHOOLID==78102, School:="KAP"]
Xfer.students.table[SCHOOLID==7810, School:="KAMS"]
Xfer.students.table[SCHOOLID==400146, School:="KCCP"]
Xfer.students.table[SCHOOLID==400163, School:="KBCP"]


Xfer.students.table[, School:=factor(School, levels=c("KAP", "KAMS", "KCCP", "KBCP"))]

Xfer.students.table[,Transferred:=format(ymd_hms(EXITDATE), format="%b %d, %Y")]

Xfer.students.table<-copy(Xfer.students.table[,list(School, 
                                                    Grade=GRADE_LEVEL, 
                                                    Student=LastFirst,
                                                    Transferred, 
                                                    Reason, 
                                                    Comment=EXITCOMMENT)][order(School, Transferred)]
)

