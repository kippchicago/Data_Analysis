library(ggplot2)
library(gridExtra)
library(plyr)
library(reshape)

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







Q12<-read.csv('/Users/chaid/Dropbox/Consulting/KIPP Ascend/Data Analysis/Q12/Q12_All_Responces_121113.csv')


Q12<-Q12[,c(1,5:16)]

quests<-names(Q12)[-1]


for(n in quests){
  Q12[,n] <- factor(x=Q12[,n], levels=c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"))
  Q12[,n] <- ordered(x=Q12[,n])
}


Q12.list<-as.list(Q12[,-1])
for(i in 1:length(Q12.list)){
  Q12.list[[i]]<-Q12.list[[i]][!is.na(Q12.list[[i]])]
  levels(Q12.list[[i]])<-c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
}

#Change list attributes to full question statements (yes, that is oxymoronic)
names(Q12.list)<-c(
                  "1. I know what is expected of me at work.",
                  "2. I have the materials and equipment needed to do my work.",
                  "3. At work I have the opportunity to do what I do best everyday.",
                  "4. In the last seven days I have received praise or recognition at work.",
                  "5. My supervisor, or someone at work, cares about me as a person.",
                  "6. Someone at work encourages my development.",
                  "7. At work, my opinions seem to matter.",
                  "8. The mission of my organization makes me feel like my job is important.",
                  "9. My coworkers are committed to doing high-quality work.",
                  "10. I have a best friend at work.",
                  "11. In the last six months someone at work has talked to me about my progress.",
                  "12. In the last year I have had opportunities to learn and grow.")
Q12.graph<-net_stacked(Q12.list)
Q12.graph



##Same thing but by school
#KAPS
Q12.KAPS.list<-as.list(subset(Q12,School=="KAPS")[,-1])
for(i in 1:length(Q12.KAPS.list)){
  Q12.KAPS.list[[i]]<-Q12.KAPS.list[[i]][!is.na(Q12.KAPS.list[[i]])]
  levels(Q12.KAPS.list[[i]])<-c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
}

names(Q12.KAPS.list)<-c(
  "1. I know what is expected of me at work.",
  "2. I have the materials and equipment needed to do my work.",
  "3. At work I have the opportunity to do what I do best everyday.",
  "4. In the last seven days I have received praise or recognition at work.",
  "5. My supervisor, or someone at work, cares about me as a person.",
  "6. Someone at work encourages my development.",
  "7. At work, my opinions seem to matter.",
  "8. The mission of my organization makes me feel like my job is important.",
  "9. My coworkers are committed to doing high-quality work.",
  "10. I have a best friend at work.",
  "11. In the last six months someone at work has talked to me about my progress.",
  "12. In the last year I have had opportunities to learn and grow.")

net_stacked(Q12.KAPS.list)

#KAMS
Q12.KAMS.list<-as.list(subset(Q12,School=="KAMS")[,-1])
for(i in 1:length(Q12.KAMS.list)){
  Q12.KAMS.list[[i]]<-Q12.KAMS.list[[i]][!is.na(Q12.KAMS.list[[i]])]
  levels(Q12.KAMS.list[[i]])<-c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
}

names(Q12.KAMS.list)<-c(
  "1. I know what is expected of me at work.",
  "2. I have the materials and equipment needed to do my work.",
  "3. At work I have the opportunity to do what I do best everyday.",
  "4. In the last seven days I have received praise or recognition at work.",
  "5. My supervisor, or someone at work, cares about me as a person.",
  "6. Someone at work encourages my development.",
  "7. At work, my opinions seem to matter.",
  "8. The mission of my organization makes me feel like my job is important.",
  "9. My coworkers are committed to doing high-quality work.",
  "10. I have a best friend at work.",
  "11. In the last six months someone at work has talked to me about my progress.",
  "12. In the last year I have had opportunities to learn and grow.")

net_stacked(Q12.KAMS.list)


#KCCP
Q12.KCCP.list<-as.list(subset(Q12,School=="KCCP")[,-1])
for(i in 1:length(Q12.KCCP.list)){
  Q12.KCCP.list[[i]]<-Q12.KCCP.list[[i]][!is.na(Q12.KCCP.list[[i]])]
  levels(Q12.KCCP.list[[i]])<-c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
}

names(Q12.KCCP.list)<-c(
  "1. I know what is expected of me at work.",
  "2. I have the materials and equipment needed to do my work.",
  "3. At work I have the opportunity to do what I do best everyday.",
  "4. In the last seven days I have received praise or recognition at work.",
  "5. My supervisor, or someone at work, cares about me as a person.",
  "6. Someone at work encourages my development.",
  "7. At work, my opinions seem to matter.",
  "8. The mission of my organization makes me feel like my job is important.",
  "9. My coworkers are committed to doing high-quality work.",
  "10. I have a best friend at work.",
  "11. In the last six months someone at work has talked to me about my progress.",
  "12. In the last year I have had opportunities to learn and grow.")

net_stacked(Q12.list)

t1<-textGrob("KIPP Chicago")
t2<-textGrob("KIPP Ascend Primary School")
t3<-textGrob("KIPP Ascend Middle School")
t4<-textGrob("KIPP Create College Prep")


tmp <- ggplot_gtable(ggplot_build(net_stacked(Q12.KCCP.list)))
leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
legend <- tmp$grobs[[leg]]


#Plot each separately. 
grid.arrange(arrangeGrob(t1, net_stacked(Q12.list)+theme(legend.position="none"), t2, net_stacked(Q12.KAMS.list)+theme(legend.position="none"), t3, net_stacked(Q12.KAPS.list)+theme(legend.position="none"), t4, net_stacked(Q12.KCCP.list)+theme(legend.position="none"), ncol=1, heights=c(.5,4,.5,4,.5,4)), legend, widths=unit.c(unit(1, "npc") - legend$widths[2], legend$widths[2]),  ncol=2)



