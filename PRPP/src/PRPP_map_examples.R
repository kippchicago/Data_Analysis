library(ProjectTemplate)
load.project()


pdf(file=paste0("graphs/PRPP_examples.pdf"), width=10.5, height=8)
  arrowdiagr(MAP.Network.School.Level.data[Sub_Test_Name=="Reading" & 
                                             Growth_Grade_Level==8],
             t="NWEA MAP: Percent Meeting/Exceeding Growth Targets \n Fall 11 - Spring 12 \n 8th Reading", 
             ranking="PctGrowth", masked=T)
dev.off()