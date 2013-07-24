require(ProjectTemplate)
load.project()

#add y label position for table
ISAT.plotdata[School=="CPS",y.label.pos:=0.25]
ISAT.plotdata[School=="KAMS",y.label.pos:=-0.25]
ISAT.plotdata[School=="KCCP",y.label.pos:=0]

# get maxs and mins. NB: only using kams for now, since they are only KChi 
# time series.
# TO DO.  Write function that gets this for each grouping variable (School) &
#         Then attaches to appropriate data.table
ISAT.maxmin<-rbind(ISAT.kams.plotdata[Year>=2007,list(value=min(value), grp="min"), by=list(variable, Grade)],ISAT.kams.plotdata[Year>=2007,list(value=max(value), grp="max"), by=list(variable, Grade)])

setkey(ISAT.maxmin, variable, Grade, value)
setkey(ISAT.kams.plotdata, variable, Grade, value)

ISAT.maxmin<-ISAT.kams.plotdata[ISAT.maxmin]

#plot



ISAT.plot<-ggplot(ISAT.plotdata[Year>=2007], 
                  ISaes(x=Year, y=value)) +
  geom_line(aes(group=School, color=School)) + 
  geom_point(data=ISAT.plotdata[School=="KCCP"], 
             aes(x=Year, y=value, color=School), shape=18, size=3.5) +
  geom_point(data=ISAT.maxmin, aes(x=Year, y=value, color=grp)) + 
  facet_grid(Grade ~ variable) + 
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.text.x= element_text(size= 8,angle=35),
        plot.margin = unit(c(0.5, 0, 0.5, 0.5), "lines")
  ) +
  scale_color_manual(values = c("#E27425", #KAMS
                                "#439539", #CPS 
                                "#60A2D7", #KCCP
                                "#8D8685", #max
                                "#8D8685" #min
  )
  )




ISAT.tbl<-ggplot(ISAT.plotdata[Year>=2007], 
                 aes(x=Year, y=y.label.pos)) + 
  geom_text(aes(label=round(value), color=School), size=3.5) + 
  facet_grid(Grade~variable) + 
  #theme_bw +
  theme(panel.grid.major = element_blank(), 
        #panel.border = element_blank(), 
        axis.text.y =  element_blank(),
        axis.ticks =  element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(), 
        axis.text.x= element_text(size= 8,angle=35),
        #strip.background = element_blank(),
        legend.position="right",
        plot.margin = unit(c(0.5, 0.5, 0.5, -1), "lines")
  ) + 
  ylim(0.5, -0.5) +
  scale_color_manual(values = c("#E27425", #KAMS
                                "#439539", #CPS 
                                "#60A2D7", #KCCP
                                "#BCD631", #max
                                "#BCD631" #min
  )
  )






#in Rows
gPlot <- ggplotGrob(ISAT.plot)
gTbl <- ggplotGrob(ISAT.tbl)


maxHeight = grid::unit.pmax(gPlot$heights[2:3], gTbl$heights[2:3])
gPlot$heights[2:3] <- as.list(maxHeight)
gTbl$heights[2:3] <- as.list(maxHeight)


grid.arrange(gPlot, gTbl, ncol=2, widths=c(1,2))


pdf(file="graphs//ISAT_sparklines.pdf", width=11, height=8.5)
grid.arrange(gPlot, gTbl, ncol=2, widths=c(1,2))
dev.off()


########################
## 5-8 Combined only ##
#######################

ISAT.58.Comp.plot<-ggplot(ISAT.plotdata[Year>=2007 & Grade=="5-8"], 
                  aes(x=Year, y=value)) +
  geom_line(aes(group=School, color=School)) + 
  geom_point(data=ISAT.plotdata[School=="KCCP" & Grade=="5-8"], 
             aes(x=Year, y=value, color=School), shape=18, size=3.5) +
  geom_point(data=ISAT.maxmin[Grade=="5-8"], aes(x=Year, y=value, color=grp)) + 
  facet_grid(Grade ~ variable) + 
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.text.x= element_text(size= 8,angle=35),
        plot.margin = unit(c(0.5, 0, 0.5, 0.5), "lines")
  ) +
  scale_color_manual(values = c("#E27425", #KAMS
                                "#439539", #CPS 
                                "#60A2D7", #KCCP
                                "#8D8685", #max
                                "#8D8685" #min
                                )
                     )




ISAT.58.Comp.tbl<-ggplot(ISAT.plotdata[Year>=2007 & Grade=="5-8"], 
                 aes(x=Year, y=y.label.pos)) + 
  geom_text(aes(label=round(value), color=School), size=3.5) + 
  facet_grid(Grade~variable) + 
  #theme_bw +
  theme(panel.grid.major = element_blank(), 
        #panel.border = element_blank(), 
        axis.text.y =  element_blank(),
        axis.ticks =  element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(), 
        axis.text.x= element_text(size= 8,angle=35),
        #strip.background = element_blank(),
        legend.position="right",
        plot.margin = unit(c(0.5, 0.5, 0.5, -1), "lines")
  ) + 
  ylim(0.5, -0.5) +
  scale_color_manual(values = c("#E27425", #KAMS
                                "#439539", #CPS 
                                "#60A2D7", #KCCP
                                "#BCD631", #max
                                "#BCD631" #min
                                )
                     )






#in Rows
gPlot <- ggplotGrob(ISAT.58.Comp.plot)
gTbl <- ggplotGrob(ISAT.58.Comp.tbl)


maxHeight = grid::unit.pmax(gPlot$heights[2:3], gTbl$heights[2:3])
gPlot$heights[2:3] <- as.list(maxHeight)
gTbl$heights[2:3] <- as.list(maxHeight)


grid.arrange(gPlot, gTbl, ncol=2, widths=c(1,2))

