#a function that returns student number based on the best match

best_match <- function(input_column, .data, match_column, output_column, confidence=.8){

  m_col<-as.character(substitute(match_column))
  o_col<-as.character(substitute(output_column))  
  
  inner <- function(input_name, ...){
    if(!require(RecordLinkage)) stop("Please install package RecordLinakge.")
    #fuzzy match on strings
  
    

  
  
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
    #out <- max(.data[which(.data[,m_col]==best),o_col])
    out <- .data[which(.data[,m_col]==best),o_col]
    if(is.factor(out)) out<-as.character(out)
    #return NA if below confidence
    if (max(distance) >= confidence) {
      return(out)
    } else {
      return(NA)
    }
  }
  
  x<-mapply(FUN = inner,
            as.list(as.character(input_column)), 
            MoreArgs = list(.data=.data, 
                            m_col=m_col,
                            o_col=o_col,
                            confidence=confidence
                            ),
            SIMPLIFY=TRUE
            )
  #x<-unlist(x)
  x
}