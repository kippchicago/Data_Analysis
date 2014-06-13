prepSurveyAvg <- function(.data=hsr, school="KIPP Ascend Primary School"){
  require(mapvisuals)
  stopifnot(is.data.table(.data))
  x<-.data[Measure.Names=="Average"]
  x.school <- x[School==school]
  x.network <- x[School=="KIPP Network"]
  
  setkey(x.school, Survey.Question, Parent.Topic, Topic.Name, Role, Measure.Names)
  setkey(x.network, Survey.Question, Parent.Topic, Topic.Name, Role, Measure.Names)
  
  x.out<-x.network[x.school]
  
  x.out[, Magnitude:=Measure.Values.1/Measure.Values]
  x.out[, Difference:=Measure.Values.1-Measure.Values]
  
  school.name <-  abbrev(school, exceptions = list(old=c("KAPS", "KCMS"), 
                                                   new=c("KAP", "KCCP")))
  setnames(x.out, 
           c("Measure.Values", "Measure.Values.1"),
           c("Network", school.name)
           )
  
  x.out[,c("School", "School.1"):=NULL]
  

  
  x.out[, Role2:=factor(Role, levels=c("Student",
                                                "Parent",
                                                "Teacher",
                                                "School Leader",
                                                "Staff"
                                       )
                        )
        ]
  
  x.out
}

plotMAPRegs<-function(.data=map.combined, 
                      grade="5th Grade", 
                      subject="Mathematics",
                      guides=FALSE){
  plot.data<-.data[Grade==grade & Subtest.Name==subject]
  
  
  regions<-plot.data[level2=="Region", as.character(Region)][order(plot.data[level2=="Region",Pct_ME])]
  
  plot.data[,Region:=factor(as.character(Region),levels=regions)]
  
  
  p<-ggplot(plot.data, 
            aes(x=Pct_ME, y=Region)) + 
    geom_point(aes(shape=level, size=level, alpha=level)) +
    geom_point(data=plot.data[Region=="KIPP Chicago"],
               aes(shape=level, 
                   size=level,
                   alpha=level),
               color="orange")+
    scale_size_manual(values = c(3,2)) +
    scale_alpha_manual(values = c(1,.5)) +
    #geom_text(aes(x=min(Pct_ME)-2, label=KIPP.Region)) +
    facet_grid(Grade~Subtest.Name, scales = "free_y", space="free_y") +
    theme_bw() + 
    xlab("% Meets/Exceeds Typical Growth")
  if(!guides) p<- p+ guides(color="none", 
                            alpha="none", 
                            size="none", 
                            shape="none")
  p
  
}

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