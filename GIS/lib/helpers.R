#a function that returns student number based on the best match
best_match <- function(input_name, .data, match_column, output_column, confidence){
  if(!require(RecordLinkage)) stop("Please install package RecordLinakge.")
  #fuzzy match on strings
  
  
  m_col<-as.character(substitute(match_column))
  o_col<-as.character(substitute(output_column))
  
  
  .data[,m_col]
  
  distance <- levenshteinSim(input_name, 
                             as.character(.data[,m_col]))
  
  best <- .data[, m_col][distance == max(distance)]
  
  #name of matc
  #match_name <- max(.data[which(.data[,m_col]==best), m_col])
  
  #display to console if not an exact match
  if (max(distance) < 1.0) {
    #build string
    disp_str <- paste(
      input_name , 'matched to',
      best, 'with confidence', round(max(distance), 2), '\n'
    )
    #print 
    message(disp_str)
  }
  
  #get student number from attrition data frame that matches
  out <- max(.data[which(.data[,m_col]==best),o_col])
  
  #return NA if below confidence
  if (max(distance) >= confidence) {
    return(out)
  } else {
    return(NA)
  }
}

gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}



