net_stacked<-function(x) {
  
  ## x: a data.frame or list, where each column is a ordered factor with the same levels
  ## lower levels are presumed to be "negative" responses; middle value presumed to be neutral
  ## returns a ggplot2 object of a net stacked distribution plot
  
  require(ggplot2)
  
  ## Test that all elements of x have the same levels, are ordered, etc.
  all_levels <- levels(x[[1]])
  n <- length(all_levels)
  levelscheck <- all(sapply(x, function(y)
    all(c(is.ordered(y), levels(y) == all_levels))
  ))
  if(!levelscheck)
    stop("All levels of x must be ordered factors with the same levels")
  
  ## Reverse order of columns (to make ggplot2 output look right after coord_flip)
  x <- x[length(x):1]
  
  ## Identify middle and "negative" levels
  if(n %% 2 == 1)
    neutral <- all_levels[ceiling(n/2)]
  else
    neutral <- NULL
  
  negatives <- all_levels[1:floor(n/2)]
  positives <- setdiff(all_levels, c(negatives, neutral))
  
  ## remove neutral, summarize as proportion
  listall <- lapply(names(x), function(y) {
    column <- (na.omit(x[[y]]))
    out <- data.frame(Question = y, prop.table(table(column)))
    names(out) <- c("Question", "Response", "Freq")
    
    if(!is.null(neutral))
      out <- out[out$Response != neutral,]
    
    out
  })
  
  dfall <- do.call(rbind, listall)
  
  ## split by positive/negative
  pos <- dfall[dfall$Response %in% positives,]
  neg <- dfall[dfall$Response %in% negatives,]
  
  ## Negate the frequencies of negative responses, reverse order
  neg$Freq <- -neg$Freq
  neg$Response <- ordered(neg$Response, levels = rev(levels(neg$Response)))
  
  ## Make sure Question are in same order as x
  pos$Question<-factor(pos$Question, levels=names(x))
  neg$Question<-factor(neg$Question, levels=names(x))
  
  stackedchart <- ggplot() +
    aes(Question, Freq, fill = Response, order = Response) + 
    geom_bar(data = neg, stat = "identity") +
    geom_bar(data = pos, stat = "identity") + geom_hline(yintercept=0) +
    scale_y_continuous(name = "",
                       labels = paste0(seq(-40, 100, 20), "%"),
                       limits = c(-.4, 1),
                       breaks = seq(-.4, 1, .2)) +
    scale_fill_manual(limits = c(negatives, positives), values=c("#E27425", "#FEBC11", "#A7CFEE","#255694")) +
    coord_flip()
  
  stackedchart
}