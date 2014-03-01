inset_zoom<-function(
  p,    	# ggplot object					######
  zoom = 2,					# The zoom level of the expand box			######
  pad = 1,						# Padding between exp box and plot box		######
  ex.col = "white",			# Fill colour of expand boxes				######
  ex.alpha = 0,				# Turn to zero to turn off any colour		######
  ex.lin = "black",			# Line colour of expand boxes				######
  x.expand = c(3, 4),			# Desired expand range (X)					######
  y.expand = c(13, 16),		# Desired expand range (y)					######
  j.expand = "br"	){
  
  p.grob<-ggplot_build(p)
  xlims <- p.grob$panel$ranges[[1]]$x.range
  ylims <- p.grob$panel$ranges[[1]]$y.range
  
  ##########################################################
  # DO NOT EDIT - Computation of expanded box coords #######
  ##########################################################
  if (j.expand == "tr") {  							######
                                  xl = xlims[2]-((x.expand[2]-x.expand[1])*zoom)		######
                                  xh = xlims[2]										######
                                  yl = ylims[2]-((y.expand[2]-y.expand[1])*zoom)		######
                                  yh = ylims[2]										######
                                  padl = -1											######
                                  padr = pad											######
                                  padt = pad											######
                                  padb = -1											######
  }													######
  ######
  if (j.expand == "bl") {								######
                                 xl = xlims[1]										######
                                 xh = xlims[1]+((x.expand[2]-x.expand[1])*zoom)		######
                                 yl = ylims[1]										######
                                 yh = ylims[1]+((y.expand[2]-y.expand[1])*zoom)		######
                                 padl = pad-1										######
                                 padr = -1											######
                                 padt = -1											######
                                 padb = pad-1										######
  }													######
  ######
  if (j.expand == "tl") {								######
                                 xl = xlims[1]										######
                                 xh = xlims[1]+((x.expand[2]-x.expand[1])*zoom)		######
                                 yl = ylims[2]-((y.expand[2]-y.expand[1])*zoom)		######
                                 yh = ylims[2]										######
                                 padl = pad-1										######
                                 padr = -1											######
                                 padt = pad											######
                                 padb = -1											######
  }													######
  ######
  if (j.expand == "br") {								######
                                 xl = xlims[2]-((x.expand[2]-x.expand[1])*zoom)		######
                                 xh = xlims[2]										######
                                 yl = ylims[1]										######
                                 yh = ylims[1]+((y.expand[2]-y.expand[1])*zoom)		######
                                 padl = -1											######
                                 padr = pad											######
                                 padt = -1											######
                                 padb = pad-1										######
  }													######
  ##########################################################
  ##########################################################
  ##########################################################
  
  #here we do that acutal zooming
  p.zoom <- p +
    coord_cartesian (xlim = x.expand, ylim = y.expand) +
    annotate("text",   					#IMPORTANT
             x=x.expand[2], 						#IMPORTANT
             y = y.expand[1], 					#IMPORTANT
             label = paste("x", zoom, sep=""), 	#IMPORTANT
             hjust = 1.25, 						#IMPORTANT
             vjust = -0.4						#IMPORTANT
    ) +				
    theme (
      # Plot Attributes
      plot.title = element_text (size= 10),										#IMPORTANT
      plot.margin = unit (c(padt, padr, padb, padl), "lines"), #T, R, B, L		#IMPORTANT
      plot.background = element_blank (), 										#IMPORTANT
      
      # Panel Attributes	
      panel.grid.major = element_blank (), 										# Removes major grid
      panel.grid.minor = element_blank (),  										# Removes minor grid
      panel.background = element_rect (fill = ex.col),							#IMPORTANT
      panel.border = element_rect (colour = ex.lin, fill = F, size = 0.5),		#IMPORTANT	
      
      #Legend Attributes		
      legend.position = "none",											#IMPORTANT
      
      
      # Axis Attributes
      axis.title.x = element_blank (),	#IMPORTANT
      axis.title.y = element_blank (), 	#IMPORTANT
      axis.text.x = element_blank (), 	#IMPORTANT
      axis.text.y = element_blank (), 	#IMPORTANT
      axis.ticks = element_blank ()		#IMPORTANT
    )
  
  p.zoom <- ggplotGrob (p.zoom)
  
  #add these to orginial plot
  # area zoomed added to original plot
  p + annotate("rect",   		#IMPORTANT
               xmin = x.expand[1], 	#IMPORTANT
               xmax = x.expand[2], 	#IMPORTANT
               ymin = y.expand[1], 	#IMPORTANT
               ymax = y.expand[2], 	#IMPORTANT
               fill = ex.col, 			#IMPORTANT
               colour = ex.lin,		#IMPORTANT
               alpha = ex.alpha		#IMPORTANT
  ) + 
    annotation_custom(p.zoom, 
                      xmin=xl, 
                      xmax=xh, 
                      ymin=yl, 
                      ymax=yh)  
}