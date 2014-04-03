kipp.dt<-copy(AvgRITbySchool.2014.04.01)

# Fixx Grades to a factor with proper ordering 
kipp.dt[,Grade:=gsub("([:1-9:]|K).+","\\1",as.character(Grade))]
kipp.dt[Grade=="K", Grade:="0"]
kipp.dt[,Grade:=as.integer(Grade)]


k.s12<-kipp.dt[,list(Subtest.Name, 
                     School.ID,
                     School,
                     KIPP.Region,
                     Grade,
                     S2012_Avg.RIT.Score, 
                     S2012_N
)
]
k.s12[,Grade:=as.integer(Grade+1)]              

k.f12s13<-copy(kipp.dt)
k.f12s13[,c("S2012_Avg.RIT.Score", "S2012_N"):=NULL]

k.dt<-inner_join(k.s12, k.f12s13, by=c("Subtest.Name", 
                                       "School.ID", 
                                       "School",  
                                       "KIPP.Region",
                                       "Grade"))
k.dt<-data.table(k.dt)

k.dt[,summer_loss:= F2012_Avg.RIT.Score-S2012_Avg.RIT.Score]
k.dt[,s2s_diff:= S2013_Avg.RIT.Score-S2012_Avg.RIT.Score]


k.dt[,rank_loss:=rank(-summer_loss), by=list(Grade, Subtest.Name)]
k.dt[,rank_s2s_gain:=rank(-s2s_diff), by=list(Grade, Subtest.Name)]