plot_lickert <- function(.data, topic="Instructional Leadership"){
  data<-.data[Topic.Name %in% topic]
  
  hsr.sum.pos.neg<-data[,list(Sum=sum(Measure.Values)), 
                                 by=list(Topic.Name, 
                                         SQ, 
                                         School, Sign)]
  hsr.sum.pos.neg[Sign=="Strongly Agree", x:=1.05]
  hsr.sum.pos.neg[Sign=="Strongly Disagree", x:=-0.85]
  hsr.sum.pos.neg[Sign=="Neutral", x:=0]
  
  data.labels<-hsr.sum.pos.neg[Topic.Name %in% topic]
  
  p<-ggplot(data[order(Measure.Names)], aes(y=School, x=x)) +
    geom_segment(aes(xend=xend, yend=School, color=Measure.Names, alpha=School), size=2.5) + 
    #geom_vline(aes(xintercept=0), type=3, color="lightgray") +
    geom_text(data=data.labels[Sign!="Neutral"],
              aes(x=x, 
                  y=School, 
                  label=round(100*Sum),
                  color=Sign, 
                  alpha=School),
              size=1.75
    ) +
    geom_text(data=data.labels[Sign=="Neutral" & Sum!=0],
              aes(x=x, 
                  y=School, 
                  label=round(100*Sum),
                  alpha=School),
              color="black",
              size=1.75
    ) +
    scale_color_brewer("", palette="RdYlGn") +
    scale_x_continuous("",limits=c(-1,1.1),
                       breaks=seq(from=-.8, to=.8, by=.2),
                       labels=c("80%", "60%", "40%", "20%", "0%", 
                                "20%", "40%", "60%", "80%")
    ) +
    scale_alpha_manual(values=c(.7,.7,1,1,1,1), guide=FALSE) + 
    facet_wrap(~SQ, ncol=1) + 
    theme_bw() + 
    theme(legend.position = "bottom",
          axis.text.y=element_text(size=5),
          plot.title=element_text(size=10)) +
    ggtitle(paste(topic, collapse = " | "))
  
  p
}