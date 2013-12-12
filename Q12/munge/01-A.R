Q12.Fall13.All[Praise.in.Last.Week=="Neural",Praise.in.Last.Week:="Neutral"]

remove.na <- function(x){
  x<-as.character(x)
  x[x==""]<-NA
  x
}
for(j in names(Q12.Fall13.All)){
  z<-paste0(j,":=remove.na(",j,")")
  Q12.Fall13.All[,eval(parse(text=z))]
}