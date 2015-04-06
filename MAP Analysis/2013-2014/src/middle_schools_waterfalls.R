require(ProjectTemplate)

load.project()

map.plot<-copy(map.F13)

#Add quartile information
map.plot<-calc_quartile(map.plot,
                        percentile.column="TestPercentile", 
                        "Fall13_Quartile")

#calculate tiered growth mulitplier
map.plot<- calc_tiered_growth(map.plot,
                              quartile.column="Fall13_Quartile", 
                              grade.column="Grade")

#Middle Schools first (since they need students by grade)
schools=list("KAMS", "KCCP", "KBCP")



lapply(schools, pdf_waterfall, .data=map.plot, 
       season="Fall13")