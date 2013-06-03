# This script creates summary data from MAP data at the KIPP Chicago Testing
# Analysis server on AWS. IT creates summary statistics for the 2011-12 and 
# 2012-13 school years (fall to spring).  The data is then saved two two csvs
# found inthe /reports folder. 

library('ProjectTemplate')
load.project()

# Summarize F12 to S13 data
map.summary.1213<-map.1213[,list(pct.meets=sum(Meets)/.N, tot.meets=sum(Meets),  pct.50th.fall=sum(Fall12_Above50th)/.N, tot.50th.fall=sum(Fall12_Above50th), pct.50th.spring=sum(Spring13_Above50th)/.N, tot.50th.spring=sum(Spring13_Above50th), tot.students=.N),by=list(Subject, Fall12_Grade)]

# Summarize F11 to S12 data
map.summary.1112<-map.1112[,list(pct.meets=sum(Meets)/.N, tot.meets=sum(Meets),  pct.50th.fall=sum(Fall11_Above50th)/.N, tot.50th.fall=sum(Fall11_Above50th), pct.50th.spring=sum(Spring12_Above50th)/.N, tot.50th.spring=sum(Spring12_Above50th), tot.students=.N),by=list(Subject, Fall11_Grade)]

# Set keys in orer to sort data
setkey(map.summary.1213, Subject, Fall12_Grade)
setkey(map.summary.1112, Subject, Fall11_Grade)

# Write summary tables to CSV
todays.date<-format(Sys.time(), "%y%m%d")

write.csv(map.summary.1213, 
          paste0('reports/KAPMS_MAP_S13_summary_',todays.date,'.csv'))
write.csv(map.summary.1112, 
          paste0('reports/KAPMS_MAP_S12_summary_',todays.date,'.csv'))

# Write student level data to csv
write.csv(map.1213, 
          paste0("reports/KAMS_MAP_S13_scores_",todays.date,".csv"))

# Students who scored lower in Spring than Fall
SpringFallNegative.1213<-map.1213[Spring13_RIT<=Fall12_RIT & Fall12_Grade>=5 & Subject!="General Science", list(StudentLastFirstName, Fall12_Grade, Subject)]

setkey(SpringFallNegative.1213, Subject, Fall12_Grade)

# Write negative spring fall student list to csv
write.csv(SpringFallNegative.1213, 
          paste0('reports/KAMS_MAP_S13_Negative_',todays.date,'.csv')

# Summarize negative Spring - Fall scores
SpringFallNegatvie.summary.1213<-SpringFallNegative.1213[,.N, by=list(Subject, Fall12_Grade)]