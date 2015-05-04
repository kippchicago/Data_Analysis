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

plotMAPRegs<-function(.data, 
                      grade="5th Grade", 
                      subject="Mathematics",
                      guides=FALSE, 
                      vary.size=FALSE, 
                      anonymize=FALSE,
                      show.above=7,
                      show.chicago=TRUE){
  require(data.table)
  .data<-data.table(.data)
  plot.data<-copy(.data[Grade==grade & Sub_Test_Name==subject])
  
  if(anonymize){
    plot.data[,Region2:=Region]
    plot.data[,Region:=Region_Anon]
    
    
    regions<-plot.data[level2=="Region", as.character(Region)][order(plot.data[level2=="Region",Pct_ME])]
    #regions<-plot.data[,as.character(Region)][order(plot.data[,Pct_ME])]
    plot.data[,Region:=factor(as.character(Region),levels=regions)]
    
    max.rank<-max(as.numeric(plot.data$Region))
    plot.data[as.numeric(Region) %in% c((max.rank-show.above):max.rank), Region:=Region2]
    
    if(show.chicago) plot.data[Region_Name=="KIPP Chicago", Region:=Region2]
    
    regions<-plot.data[level2=="Region", as.character(Region)][order(plot.data[level2=="Region",Pct_ME])]
    plot.data[,Region:=factor(as.character(Region),levels=regions)]
    
  } else {
    regions<-plot.data[level2=="Region", as.character(Region)][order(plot.data[level2=="Region",Pct_ME])]
    
    plot.data[,Region:=factor(as.character(Region),levels=regions)]  
  }
  
  
  p<-ggplot(plot.data, 
            aes(x=Pct_ME, y=Region))
  
  if(!vary.size) {
    p <- p + geom_point(aes(shape=level, size=level, alpha=level), 
                        color="#C49A6C") +
      geom_point(data=plot.data[Region=="KIPP Chicago"],
                 aes(shape=level, 
                     size=level,
                     alpha=level),
                 color="purple")+
      scale_size_manual(values = c(3,1.5)) +
      scale_alpha_manual(values = c(.75,.5))
  } else {
    p <- p + geom_point(aes(shape=level, size=N, alpha=level),
                        color="#C49A6C") +
      geom_point(data=plot.data[Region=="KIPP Chicago"],
                 aes(shape=level, 
                     size=N,
                     alpha=level),
                 color="purple") +
      #scale_size_manual(values = c(3,2)) +
      scale_alpha_manual(values = c(.8,.4))  +
      scale_size_continuous("Total\nStudents", 
                            range = c(1, 7), 
                            breaks=c(50,100,250,500,1000,2000)) 
  }
  p<- p + facet_grid(Grade~Sub_Test_Name, scales = "free_y", space="free_y") +
    scale_x_continuous(breaks=c(20, 40, 60, 80), 
                       labels=c("20%", "40%", "60%", "80%")
    ) +
    theme_bw() 
  if(!guides) p<- p+ guides(color="none", 
                            alpha="none", 
                            size="none", 
                            shape="none")
  p + xlab("Percent Meets/Exceeds Typcial Growth")
  
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


plot_lickert_2 <- function(.data, topic="Instructional Leadership"){
  data<-.data %>% 
    dplyr::filter(topic_name %in% topic) %>% 
    pre_process_lickert

    
  
  
  hsr.sum.pos.neg<-data %>%
    group_by(topic_name, SQ, School, Sign) %>%
    summarize(Sum=sum(measure_values)) %>%
    mutate(x=ifelse(Sign=="Strongly Agree", 1.05, NA),
           x=ifelse(Sign=="Strongly Disagree", -0.85, x),
           x=ifelse(Sign=="Neutral", 0, x)
           )

  
  data.labels<-hsr.sum.pos.neg
  
  p<-ggplot(data %>% arrange(Measure.Names), 
            aes(y=School, x=x)) +
    geom_segment(aes(xend=xend, 
                     yend=School, 
                     color=Measure.Names, 
                     alpha=School), size=2.5) + 
    #geom_vline(aes(xintercept=0), type=3, color="lightgray") +
    geom_text(data=data.labels %>% filter(Sign!="Neutral"),
              aes(x=x, 
                  y=School, 
                  label=round(100*Sum),
                  color=Sign, 
                  alpha=School),
              size=1.75
    ) +
    geom_text(data=data.labels %>% filter(Sign=="Neutral" & Sum!=0),
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


pre_process_lickert <- function(.data) {
  #subset lickert measures
  lick.levels<-c("Strongly Disagree",
                 "Disagree",
                 "Neutral",
                 "Agree",
                 "Strongly Agree")
  
  hsr.lickert<-.data %>% 
    filter(measure_names %in% lick.levels) %>%
    mutate(Measure.Names=factor(measure_names, levels=lick.levels, ordered=TRUE))
  
  #identify center level
  lick.names.len<-length(levels(hsr.lickert$Measure.Names))
  center<-(lick.names.len-1)/2 + 1 
  center.name<-levels(hsr.lickert$Measure.Names)[center]
  neg.names <- levels(hsr.lickert$Measure.Names)[1:(center-1)]
  pos.names <- levels(hsr.lickert$Measure.Names)[(center+1):lick.names.len]
  
  
  neu<-hsr.lickert %>% filter(Measure.Names %in% center.name)
  pos<-hsr.lickert %>% 
    filter(Measure.Names %in% pos.names) %>%
    arrange(Measure.Names) %>%
    group_by(school, survey_question) %>%
    mutate(x=cumsum(measure_values)-measure_values,
           xend=x+measure_values
           )
  
  
  
  
  neg<-hsr.lickert %>% 
    filter(Measure.Names %in% neg.names) %>%
    arrange(desc(Measure.Names)) %>%
    group_by(school, survey_question) %>%
    mutate(x=cumsum(measure_values)-measure_values,
           xend=x+measure_values,
           x=-x,
           xend=-xend)
    
  neu<-neu %>% mutate(x=-measure_values/2,
                 xend=-x)
  
  
  nn<-left_join(neg, 
                neu %>% select(school, survey_question, adj=x), 
                by = c("school", "survey_question")) %>%
    mutate(x=x+adj,
           xend=xend+adj,
           adj=NULL)
  
  neg<-nn
  
  pn<-left_join(pos, 
                neu %>% select(school, survey_question, adj=xend), 
                by = c("school", "survey_question")) %>%
    mutate(x=x+adj,
           xend=xend+adj,
           adj=NULL
    )
  pos<-pn
  
  rm(nn)
  rm(pn)
  
  hsr.plot.data<-rbind(pos, neg, neu) %>%
    mutate(Measure.Names=factor(as.character(Measure.Names), levels=lick.levels, ordered=TRUE),
           School=mapvisuals::abbrev(school, exceptions=list(old=c("KAPS", "KCMS"), new=c("KAP", "KCCP"))),
           School=factor(School, levels=rev(c("KAP", "KAMS", "KCCP", "KBCP", "KC", "KN"))),
           Sign=ifelse(grepl("Agree", Measure.Names),"Strongly Agree", NA),
           Sign=ifelse(grepl("Disagree", Measure.Names), "Strongly Disagree", Sign),
           Sign=ifelse(grepl("Neutral", Measure.Names),"Neutral", Sign),
           SQ=stringr::str_wrap(survey_question, width=90)
    )
  
#   hsr.sum.pos.neg<-hsr.plot.data[,list(Sum=sum(Measure.Values)), 
#                                  by=list(Topic.Name, 
#                                          SQ, 
#                                          School, Sign)]
#   hsr.sum.pos.neg[Sign=="Strongly Agree", x:=1.05]
#   hsr.sum.pos.neg[Sign=="Strongly Disagree", x:=-0.85]
#   hsr.sum.pos.neg[Sign=="Neutral", x:=0]
  
hsr.plot.data
}
