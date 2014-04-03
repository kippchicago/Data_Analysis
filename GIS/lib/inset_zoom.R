# Function takes a ggplot object, the x and y extents of the area to be zoomed, inset placement, and a 
# a zoom factor; it returns a ggplot object with the zoomed area outlined 
# and inset of zoomed area properly place.  Original idea and large swathes of code from 
# Darren Wilkinson (https://wilkinsondarren.wordpress.com/2013/11/26/ggplot-expanded-plots)

inset_zoom<-function(
  p,    	              # ggplot object
  x.expand = c(3, 4),  	# target zoom range (X)
  y.expand = c(13, 16),	# target zoom range (y)
  j.expand = "br"	,     # inset location
  zoom = 2,					    # The zoom level of the inset box
  inset.title = "",
  pad = 1,						  # Padding between inset box and plot box
  ex.col = "white",			# Fill color of inset boxes
  ex.alpha = 0,				  # Turn to zero to turn off any colour
  ex.lin = "black"			# Line colour of expand boxes
  ){
  
  # Get x and y limits from ggplot object
  p.grob<-ggplot_build(p)
  xlims <- p.grob$panel$ranges[[1]]$x.range
  ylims <- p.grob$panel$ranges[[1]]$y.range
  
  
  # Computation of inset box coords (note zooming makes the box bigger) based on 
  # placement t=top, b=bottom, r=right, l=left
  if (j.expand == "tr") {
    xl = xlims[2]-((x.expand[2]-x.expand[1])*zoom)
    xh = xlims[2]										
    yl = ylims[2]-((y.expand[2]-y.expand[1])*zoom)		
    yh = ylims[2]										
    padl = -1											
    padr = pad										
    padt = pad										
    padb = -1											
  }													
  
  if (j.expand == "bl") {
    xl = xlims[1]
    xh = xlims[1]+((x.expand[2]-x.expand[1])*zoom)
    yl = ylims[1]										
    yh = ylims[1]+((y.expand[2]-y.expand[1])*zoom)		
    padl = pad-1										
    padr = -1										
    padt = -1										
    padb = pad-1								
  }													
  
  if (j.expand == "tl") {
    xl = xlims[1]										
    xh = xlims[1]+((x.expand[2]-x.expand[1])*zoom)		
    yl = ylims[2]-((y.expand[2]-y.expand[1])*zoom)		
    yh = ylims[2]										
    padl = pad-1										
    padr = -1											
    padt = pad										
    padb = -1											
  }													
  
  if (j.expand == "br") {								
    xl = xlims[2]-((x.expand[2]-x.expand[1])*zoom)	
    xh = xlims[2]			
    yl = ylims[1]
    yh = ylims[1]+((y.expand[2]-y.expand[1])*zoom)
    padl = -1
    padr = pad
    padt = -1
    padb = pad-1
  }													
  
  
  # Do the actual zooming by chaning the cartesian coordinates of the orignal
  # ggplot object
  p.zoom <- p +
    coord_cartesian (xlim = x.expand, ylim = y.expand) +
    annotate("text",   					
             x=x.expand[2], 						
             y = y.expand[1], 					
             label = paste("x", zoom, sep=""), 	
             hjust = 1.25, 						
             vjust = -0.4						
    ) +				
    theme (
      # Plot Attributes
      plot.title = element_text (size= 10),										
      plot.margin = unit (c(padt, padr, padb, padl), "lines"), #top, right, bottom, left		
      plot.background = element_blank (), 										
      
      # Panel Attributes	
      panel.grid.major = element_blank (), 										
      panel.grid.minor = element_blank (),  									
      panel.background = element_rect (fill = ex.col),							
      panel.border = element_rect (colour = ex.lin, fill = F, size = 0.5),			
      
      #Legend Attributes		
      legend.position = "none",											
      
      
      # Axis Attributes
      axis.title.x = element_blank (),	
      axis.title.y = element_blank (), 	
      axis.text.x = element_blank (), 	
      axis.text.y = element_blank (), 	
      axis.ticks = element_blank ()		
    ) + 
    ggtitle(inset.title) #without this p.zoom inherits p's title 
  
  # change ggplot object ot grid object to use in annotate custom
  p.zoom <- ggplotGrob (p.zoom)
  
  #add these to orginial plot
  # area zoomed added to original plot
  p + annotate("rect",   		
               xmin = x.expand[1], 	
               xmax = x.expand[2], 	
               ymin = y.expand[1], 	
               ymax = y.expand[2], 	
               fill = ex.col, 			
               colour = ex.lin,		
               alpha = ex.alpha		
  ) + 
    annotation_custom(p.zoom, 
                      xmin=xl, 
                      xmax=xh, 
                      ymin=yl, 
                      ymax=yh)  
}