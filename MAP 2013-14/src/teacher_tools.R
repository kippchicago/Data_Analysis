m.w14<-map.all[TermName=="Winter 2013-2014"]

m_w13_8m<-m.w14[Grade==8 & MeasurementScale=="Mathematics"]


# Get Homerooms ####
#library(RJDBC)

# Connect to server using JDBC Connection (note: requires a VPN connection to be open to psshostingvpn.poowerschool.com)

message("Logging into server . . . ")
drvr <- JDBC("oracle.jdbc.driver.OracleDriver", "~/Dropbox/JDBC Drivers/ojdbc6.jar","") # define driver

pspw <- as.list(read.dcf("config/ps.dcf", all=TRUE)) #read DCF with configuration settings

pscon <- dbConnect(drvr,pspw$SERVER,pspw$UID,pspw$PWD)

qry<-'SELECT student_number as "StudentID", home_room FROM Students'
homerooms<-dbGetQuery(pscon, qry)

homerooms$StudentID<-as.integer(homerooms$StudentID)

#Lets take a look at goals

m.sub.scores<-m_w13_8m[,c("StudentID", "StudentFirstname",
                "StudentLastname",
                "SchoolInitials",
                "Grade",
                "MeasurementScale",
                "TestRITScore",
                "TestPercentile",
                colnames(m_w13_8m)[grep("(Goal)[0-9]RitScore", colnames(m_w13_8m))]
), with=F]


m.sub.names<-m_w13_8m[,c("StudentID", "StudentFirstname",
                          "StudentLastname",
                          "SchoolInitials",
                          "Grade",
                          "MeasurementScale",
                         "TestRITScore",
                         "TestPercentile",
                          colnames(m_w13_8m)[grep("(Goal)[0-9]Name", colnames(m_w13_8m))]
), with=F]

# melt scores
m.melt.scores<-melt(m.sub.scores, 
                    id.vars=names(m.sub.scores)[1:8], 
                    measure.vars = names(m.sub.scores)[-c(1:8)]
                    )

m.melt.names<-melt(m.sub.names, 
                   id.vars=names(m.sub.names)[1:8],
                   measure.vars = names(m.sub.names)[-c(1:8)]
                   )

assert_that(nrow(m.melt.scores)==nrow(m.melt.names))

# add homerooms
m.melt.scores2<-left_join(m.melt.scores, homerooms, by="StudentID")
assert_that(nrow(m.melt.scores)==nrow(m.melt.scores2))

m.long <- data.table(m.melt.scores2)
m.long[,Goal_Name:=m.melt.names$value]


assert_that(nrow(m.long)==nrow(m.melt.names))

m.long.2<-m.long[!is.na(Goal_Name)]
m.long.2<-m.long.2[!is.na(value)]
assert_that(nrow(m.long)>=nrow(m.long.2))




#  Boxplot ####

y_center <- min(m.long.2$value, na.rm = TRUE) + 0.5 * (max(m.long.2$value, na.rm = TRUE) - min(m.long.2$value, na.rm = TRUE)) 
goal_names <- attributes(factor(m.long.2$Goal_Name))$levels

more_30 <- nchar(goal_names) > 30

smart_breaks <- ifelse(more_30, '-\n', '')

goal_names_format <- paste(
  substr(goal_names, start = 1, stop = 30)
  ,smart_breaks
  ,substr(goal_names, start = 31, stop = 100)
  ,sep = ''
)
#a data frame of labels!

p <- ggplot(
  data = m.long.2
  ,aes(
    x = factor(Goal_Name)
    ,y = value
    ,fill = factor(Goal_Name)
  )
) +
  #empty
  geom_jitter(
    alpha = 0
  ) + 
  annotate(
    "text"
    ,x = seq(1,length(goal_names_format))
    ,y = rep(y_center, length(goal_names_format))
    ,label = goal_names_format
    ,angle = 90
    ,size = 8
    ,color = 'gray80'
    ,alpha = .9
  ) +
  geom_boxplot(
    alpha = 0.6
  ) +
  #coord_flip() + 
  geom_jitter(
    position = position_jitter(width = .15)
    ,color = 'gray85'
    ,alpha = 0.6
    ,shape = 1
  ) + 
  stat_summary(
    aes(
      label = round(..y..,1)
    )
    ,fun.y = mean
    ,geom = 'text'
    ,size = 7
  ) +
  labs(
    x = 'Goal Name'
    ,y = 'RIT Score'
  ) +
  theme(
    panel.background = element_blank()
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    #,axis.text.x = element_text(size = rel(0.7))
    ,axis.title.y = element_blank()
    ,axis.text.x = element_blank()
    ,panel.margin = unit(0,"null")
    ,plot.margin = rep(unit(0,"null"),4)
    ,axis.ticks.margin = unit(0,"null")
  )

p

### plot each kiddo ####

m.plot<-m.long.2[,Rank:=rank(TestRITScore, ties.method="first")]
m.plot[,Rank2:=rank(value, ties.method="first"), by=Goal_Name]


m.plot<-calc_quartile(m.plot, percentile.column = "TestPercentile", "TestQuartile")

m.plot[,StudentFullName:=paste(StudentFirstname, StudentLastname)]
m.plot[,StudentDisplayName:=factor(StudentFullName, levels=unique(StudentFullName)[order(Rank,decreasing = TRUE)])]

p<-list()

p$class_by_strand<-ggplot(data=m.plot, aes(y=Rank2, x=value)) +
  geom_point(aes(color=TestQuartile)) + 
  geom_text(aes(label=paste(StudentFirstname, StudentLastname), 
                color=TestQuartile), hjust=-0.3, size=2) +
  #geom_hline(aes(yintercept=mean(TestRITScore))) + 
  scale_color_discrete("Overall MAP Quartile") +
  facet_grid(.~Goal_Name) + 
  xlim(150,300) +
  xlab("RIT Score") +
  ylab("")
  theme_bw() + 
  theme(strip.text.y=element_text(angle=270), 
        legend.position="bottom")



#  Goal on Y axis ####
hrs<-m.plot[,unique(HOME_ROOM)]

p$homeroom_strands<-ggplot(data=m.plot[HOME_ROOM %in% hrs[1],], 
          aes(y=Goal_Name, 
              x=value
              )
          ) +
  geom_point(aes(fill=value-TestRITScore), 
             shape=21,
             color=NA
             ) + 
  geom_vline(aes(xintercept=mean(TestRITScore)), color="gray") + 
  geom_vline(aes(xintercept=TestRITScore, color=TestQuartile), size=1.5, show_guide=T) + 
  scale_fill_gradient("Deviation from\nOverall RIT",low="red", high="green") +
  scale_color_discrete("Overall RIT Score\nQuartile") +
  xlab("RIT Score") + 
  ylab("Strand Name") +
  facet_grid(StudentDisplayName~.) + 
  theme_bw() + # xlim(150,300) +
  theme(strip.text.y=element_text(angle=0), 
        axis.text.y=element_text(size=5)#,
        #legend.position="none"
        )


cairo_pdf("graphs/strands_test_3_1.pdf", height=8.5, width=11, onefile=T)
p$class_by_strand + ggtitle
dev.off()

cairo_pdf("graphs/strands_test_3_2.pdf", height=11, width=8.5, onefile=T)
p$homeroom_strands + ggtitle(paste("MAP and Strand RIT Scores for\n", hrs[1], sep=""))
dev.off()


