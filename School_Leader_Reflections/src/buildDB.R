#grid.newpage()
pdf("graphs/reflections_dashboard_130721.pdf", width=8.5,height=11,title="KIPP Chicago SL Relflections")

la=grid.layout(7, 2, heights=unit.c(unit(2, "lines"),
                                    unit(1, "lines"), 
                                    unit(3, "inches") -unit(3,"lines"),
                                    unit(1, "lines"), 
                                    unit(4, "inches") -unit(3,"lines"),
                                    unit(1, "lines"), 
                                    unit(3, "inches") -unit(3,"lines")
                                   ),
                          widths=unit(8/2, "inches")
                )


# Top level viewport that is 8.5"x11 inchees divided into text and 
top.vp <- viewport(layout=la)

#need to name child viewports
plottitle <- viewport(layout.pos.col = 1:2, layout.pos.row = 1,
                    name = "plottitle")
title11 <- viewport(layout.pos.col = 1, layout.pos.row = 2,
                    name = "title11")
title212 <- viewport(layout.pos.col = 1:2, layout.pos.row = 4,
                    name = "title212")
title312 <- viewport(layout.pos.col = 1:2, layout.pos.row = 6,
                    name = "title312")

title12 <- viewport(layout.pos.col = 2, layout.pos.row = 2,
                    name = "title12")

plotAtt <- viewport(layout.pos.col = 1, layout.pos.row = 3,
                    name = "plotAtt")

plotEnroll <- viewport(layout.pos.col = 2, layout.pos.row = 3,
                    name = "plotEnroll")

plotMAP <- viewport(layout.pos.col = 1:2, layout.pos.row = 5,
                    name = "plotMAP")

plotXfers <- viewport(layout.pos.col = 1:2, layout.pos.row = 7,
                    name = "plotXfers")

#plotISAT <- viewport(layout.pos.col = 1, layout.pos.row = 6,
#                    name = "plotISAT")

# Combine plots into tree
splot <- vpTree(top.vp, vpList(plottitle, title11, title212, title312,title12, plotAtt, plotEnroll, plotMAP, plotXfers))

#push vpTree
grid.newpage()
pushViewport(splot)

#Attend
seekViewport("plotAtt")
grid.rect()
grid.draw(att.gtbl)
seekViewport("title11")
grid.rect()
grid.text("Attendence", gp=gpar(fontsize=11, fontface="bold"))

# Enrollemnt
seekViewport("plotEnroll")
grid.rect()
grid.draw(Enroll.gtbl)

seekViewport("title12")
grid.rect()
grid.text("Enrollment", gp=gpar(fontsize=11, fontface="bold"))

#MAP

seekViewport("plotMAP")

pushViewport(viewport(x=0, 
                      y=1, 
                      height=.3, 
                      width=1, 
                      just=c("left", "top"), 
                      name="mapgrowth"
                      )
             )
grid.rect()

#viewport for MAP tables in each row
vpMAPplots <- function(vpname="vpMAPPlotRegion"){
  viewport(x=unit(2,"lines"), 
                  y=1, 
                  height=1, 
                  width=unit.c(unit(1,"npc")-unit(2,"lines")),
                  just=c("left", "top"),
                  name=vpname
           )
}
pushViewport(vpMAPplots("vpMapPlotGrowth"))
grid.rect()



growth.g<-arrangeGrob(map.kaps.gtbl, 
                      map.kams.gtbl, 
                      map.kccp.gtbl, 
                      map.kippchi.gtbl, 
                      nrow=1)
grid.draw(growth.g)

seekViewport("mapgrowth")

grid.text("MAP\n% Above Target", x = unit(1.2,"lines"), rot = 90, gp=gpar(fontsize=8, fontface="bold"))

seekViewport("plotMAP")

pushViewport(viewport(x=0, y=.7, height=.3, width=1, just=c("left", "top"), name="mapabove"))
grid.rect()

pushViewport(vpMAPplots("vpMapPlotAbove"))
grid.rect()

above.g<-arrangeGrob(map.kaps.above.gtbl, 
                     map.kams.above.gtbl, 
                     map.kccp.above.gtbl,
                     map.kippchi.above.gtbl, 
                     nrow=1)

grid.draw(above.g)
seekViewport("mapabove")
grid.text("MAP\n% > 50th Pctl", x = unit(1.2,"lines"), rot = 90, gp=gpar(fontsize=8, fontface="bold"))

#ISAT
seekViewport("plotMAP")



pushViewport(viewport(x=0, y=.4, height=.4, width=1, just=c("left", "top"), name="isat"))
grid.rect()

pushViewport(vpMAPplots("vpISAT"))
grid.rect()

gPlot <- ggplotGrob(ISAT.58.Comp.plot)
gTbl <- ggplotGrob(ISAT.58.Comp.tbl)


maxHeight = grid::unit.pmax(gPlot$heights[2:3], gTbl$heights[2:3])
gPlot$heights[2:3] <- as.list(maxHeight)
gTbl$heights[2:3] <- as.list(maxHeight)


isat.g<-arrangeGrob(gPlot, gTbl, ncol=2, widths=c(1,2))
grid.draw(isat.g)


seekViewport("isat")
grid.text("ISAT\n% Meets/Exceeds", x = unit(1.2,"lines"), rot = 90, gp=gpar(fontsize=8, fontface="bold"))








seekViewport("title212")
grid.rect()
grid.text("Assessments (MAP & ISAT)", gp=gpar(fontsize=11, fontface="bold"))







#Transfers
seekViewport("plotXfers")
grid.rect()
transfer.g<-arrangeGrob(transfer.plot, transfer.gtbl, nrow=1)
grid.draw(transfer.g)

seekViewport("title312")
grid.rect()
grid.text("Transfers", gp=gpar(fontsize=11, fontface="bold"))

#Title
seekViewport("plottitle")
grid.rect()
grid.text("DRAFT--KIPP Chicago School Leader Reflection Dashboard--DRAFT", gp=gpar(fontsize=12, fontface="bold"))





dev.off()
