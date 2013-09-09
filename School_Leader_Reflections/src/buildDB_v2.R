#grid.newpage()
pdf("graphs/reflections_dashboard_130827.3.pdf", width=8.5,height=11,title="KIPP Chicago SL Relflections")

#### Grid Layout ####
la=grid.layout(8, 2, heights=unit.c(unit(1.5, "lines"),
                                    unit(1.5, "lines"),
                                    unit(1, "lines"), 
                                    unit(3, "inches") -unit(3,"lines"),
                                    unit(1, "lines"), 
                                    unit(5, "inches") -unit(3,"lines"),
                                    unit(1, "lines"), 
                                    unit(2.75, "inches") -unit(3,"lines")
                                   ),
                          widths=unit(8/2, "inches")
                )


# Top level viewport that is 8.5"x11 inchees divided into text and 
top.vp <- viewport(layout=la)

#### Child Viewports ####
#need to name child viewports
plottitle <- viewport(layout.pos.col = 1:2, layout.pos.row = 1,
                    name = "plottitle")
plotkey <- viewport(layout.pos.col = 1:2, layout.pos.row = 2,
                      name = "plotkey")
title11 <- viewport(layout.pos.col = 1, layout.pos.row = 3,
                    name = "title11")
title212 <- viewport(layout.pos.col = 1:2, layout.pos.row = 5,
                    name = "title212")
title312 <- viewport(layout.pos.col = 1:2, layout.pos.row = 7,
                    name = "title312")

title12 <- viewport(layout.pos.col = 2, layout.pos.row = 3,
                    name = "title12")

plotAtt <- viewport(layout.pos.col = 1, layout.pos.row = 4,
                    name = "plotAtt")

plotEnroll <- viewport(layout.pos.col = 2, layout.pos.row = 4,
                    name = "plotEnroll")

plotMAP <- viewport(layout.pos.col = 1:2, layout.pos.row = 6,
                    name = "plotMAP")

plotXfers <- viewport(layout.pos.col = 1:2, layout.pos.row = 8,
                    name = "plotXfers")

#plotISAT <- viewport(layout.pos.col = 1, layout.pos.row = 6,
#                    name = "plotISAT")


#### Tree ####
# Combine plots into tree
splot <- vpTree(top.vp, vpList(plottitle, plotkey,title11, title212, title312,title12, plotAtt, plotEnroll, plotMAP, plotXfers))

#push vpTree
grid.newpage()
pushViewport(splot)

#### Attend ####
seekViewport("plotAtt")
grid.rect()
grid.draw(att.gtbl)
seekViewport("title11")
grid.rect()
grid.text("Attendence", gp=gpar(fontsize=11, fontface="bold"))

#### Enrollment ####
seekViewport("plotEnroll")
grid.rect()
grid.draw(Enroll.g)

grid.text("               Orange line",
          x=0, 
          y=unit(1, "lines"),
          hjust=0,
          gp=gpar(fontsize=6, fontface="italic", col="#E27425"),
          name="enrollorange")
text.cursor<-convertWidth(grobWidth("enrollorange"), "npc")
grid.text(" indicates each grade's SY2012-13 budgeted enrollment.", 
          gp=gpar(col="#8D8685", fontsize=6, fontface="italic"),
          x=text.cursor,
          y=unit(1, "lines"),
          name="enrolltext", 
          hjust=0)

seekViewport("title12")
grid.rect()
grid.text("Enrollment", gp=gpar(fontsize=11, fontface="bold"))

#### MAP ####

seekViewport("plotMAP")

pushViewport(viewport(x=0, 
                      y=1, 
                      height=.5, 
                      width=1, 
                      just=c("left", "top"), 
                      name="mapgrowth"
                      )
             )
grid.rect()
grid.text("MAP\n% Above Typical Growth", 
          x = unit(1.2,"lines"), 
          rot = 90, 
          gp=gpar(fontsize=8, fontface="bold"))
grid.text("Ranked against KIPP Network", 
          x = unit(4,"lines"), 
          rot = 90, 
          gp=gpar(fontsize=6, fontface="italic", col="#8D8685"))

grid.text("(Fall-to-Spring for K, 2, & 5; Spring-to-Spring for 1 & 6-8)", 
          x = unit(6,"lines"), 
          rot = 90, 
          gp=gpar(fontsize=5, fontface="italic", col="#8D8685"))


# viewport for MAP tables in each row
vpMAPplots <- function(vpname="vpMAPPlotRegion"){
  viewport(x=unit(3,"lines"), 
                  y=1, 
                  height=1, 
                  width=unit.c(unit(1,"npc")-unit(3,"lines")),
                  just=c("left", "top"),
                  name=vpname
           )
}
pushViewport(vpMAPplots("vpMapPlotGrowth"))
grid.rect()

grid.draw(map.bar.plot)

grid.text("            Dark orange lines ",
          x=0, 
          y=unit(1, "lines"),
          hjust=0,
          gp=gpar(fontsize=6, fontface="italic", col="#E27425"),
          name="mapnatorange")
text.cursor<-convertWidth(grobWidth("mapnatorange"), "npc")

grid.text(" indicate the national percent above typical growth (50%); ", 
          gp=gpar(col="#8D8685", fontsize=6, fontface="italic"),
          x=text.cursor,
          y=unit(1, "lines"),
          name="maptextnat", 
          hjust=0)
text.cursor<-text.cursor + convertWidth(grobWidth("maptextnat"), "npc")

grid.text("lighter orange lines ", 
          gp=gpar(col="#F7941E", fontsize=6, fontface="italic"),
          x=text.cursor,
          y=unit(1, "lines"),
          name="mapkipporange", 
          hjust=0)
text.cursor<-text.cursor + convertWidth(grobWidth("mapkipporange"), "npc")

grid.text(" indicate the KIPP Network percent above typical growth.", 
          gp=gpar(col="#8D8685", fontsize=6, fontface="italic"),
          x=text.cursor,
          y=unit(1, "lines"),
          name="maptextkipp", 
          hjust=0)



#### ISAT ####
seekViewport("plotMAP")



pushViewport(viewport(x=0, y=.5, height=.5, width=1, just=c("left", "top"), name="isat"))
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

grid.text("              Orange lines & numbers",
          x=0, 
          y=unit(1, "lines"),
          hjust=0,
          gp=gpar(fontsize=6, fontface="italic", col="#E27425"),
          name="cpsorange")
text.cursor<-convertWidth(grobWidth("cpsorange"), "npc")

grid.text(" indicates CPS performance for each grade over all schools. ", 
          gp=gpar(col="#8D8685", fontsize=6, fontface="italic"),
          x=text.cursor,
          y=unit(1, "lines"),
          name="cpstext", 
          hjust=0)
text.cursor<-text.cursor + convertWidth(grobWidth("cpstext"), "npc")

grid.text("Gray dots ", 
          gp=gpar(col="#8D8685", fontsize=6, fontface="bold.italic"),
          x=text.cursor,
          y=unit(1, "lines"),
          name="graydots", 
          hjust=0)
text.cursor<-text.cursor + convertWidth(grobWidth("graydots"), "npc")

grid.text("indicate KIPP Chicago high/low performance from SY2006-7 to SY2012-13.", 
          gp=gpar(col="#8D8685", fontsize=6, fontface="italic"),
          x=text.cursor,
          y=unit(1, "lines"),
          name="cpstext2", 
          hjust=0)







seekViewport("isat")
grid.text("ISAT\n% Meets/Exceeds", x = unit(1.2,"lines"), rot = 90, gp=gpar(fontsize=8, fontface="bold"))

isat.extext<-"Orange indicates CPS performance"
grid.text("New cut scores applied retroactively", 
          x = unit(4,"lines"), 
          rot = 90, 
          gp=gpar(fontsize=6, fontface="italic", col="#8D8685"))








seekViewport("title212")
grid.rect()
grid.text("Assessments (MAP & ISAT)", gp=gpar(fontsize=11, fontface="bold"))







#### Transfers ####
seekViewport("plotXfers")
grid.rect()
transfer.g<-arrangeGrob(transfer.plot, transfer.gtbl, nrow=1)
grid.draw(transfer.g)

grid.text("Bars and numbers show cumulative transfers.", 
          gp=gpar(col="#8D8685", fontsize=6, fontface="italic"),
          x=unit(4, "lines"),
          y=unit(1, "lines"),
          name="xfertext", 
          hjust=0)

seekViewport("title312")
grid.rect()
grid.text("Transfers", gp=gpar(fontsize=11, fontface="bold"))

##### Title ####
seekViewport("plottitle")
grid.rect()
grid.text("DRAFT--KIPP Chicago School Leader Reflection Dashboard--DRAFT", gp=gpar(fontsize=12, fontface="bold"))

#### Key ####
seekViewport("plotkey")
grid.rect()
grid.text(" Key: ", # NB: the spaces before "K" after the "y" in "Key" 
          gp=gpar(fontsize=10, fontface="bold"), 
          x=0,#unit(1, "lines"),
          name="key", 
          hjust=0)

text.cursor<-convertWidth(grobWidth("key"), "npc")
grid.text("KIPP Chicago schools indicated by color: ", 
          gp=gpar(col="#8D8685", fontsize=10, fontface="italic"),
          x=text.cursor,
          name="text1", 
          hjust=0)

text.cursor<-text.cursor + convertWidth(grobWidth("text1"), "npc")
grid.text("KAPS ",
          gp=gpar(col="purple", fontsize=10),
          x=text.cursor,
          name="kaps", 
          hjust=0)

text.cursor<-text.cursor + convertWidth(grobWidth("kaps"), "npc")
grid.text(" KAMS ",
          gp=gpar(col="#439539", fontsize=10),
          x=text.cursor,
          name="kams", 
          hjust=0)

text.cursor<-text.cursor + convertWidth(grobWidth("kams"), "npc")
grid.text(" KCCP",
          gp=gpar(col="#60A2D7", fontsize=10),
          x=text.cursor,
          name="kccp", 
          hjust=0)

text.cursor<-text.cursor + convertWidth(grobWidth("kccp"), "npc")
grid.text(". Comparative metrics indicated by ", 
          gp=gpar(col="#8D8685", fontsize=10, fontface="italic"),
          x=text.cursor,
          name="text2", 
          hjust=0)

text.cursor<-text.cursor + convertWidth(grobWidth("text2"), "npc")
grid.text("orange",
          gp=gpar(col="#E27425", fontsize=10),
          x=text.cursor,
          name="orange", 
          hjust=0)

text.cursor<-text.cursor + convertWidth(grobWidth("orange"), "npc")
grid.text(".", 
          gp=gpar(col="#8D8685", fontsize=10, fontface="italic"),
          x=text.cursor,
          name="period", 
          hjust=0)

dev.off()
