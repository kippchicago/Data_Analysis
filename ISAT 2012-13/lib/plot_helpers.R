plustable.plot <- function(plota, plotb, num.rows=2, num.cols=1) {

  #Collect layout information
  Layout <- grid.layout(nrow =num.rows , ncol = num.cols, widths = unit(c(2, 2), 
                                                          c("null", "null")))
  
  vplayout <- function(...) {
    grid.newpage()
    pushViewport(viewport(layout = Layout))
  }
  
  subplot <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)  
  
  vplayout()
  print(plota, vp = subplot(1, 1))
  print(plotb, vp = subplot(2, 1))
}

#gp1<- ggplot_gtable(ggplot_build(test.plot))
#gp2<- ggplot_gtable(ggplot_build(test.tbl))
#maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3])
#gp1$widths[2:3] <- maxWidth
#gp2$widths[2:3] <- maxWidth
#grid.arrange(gp1, gp2)
