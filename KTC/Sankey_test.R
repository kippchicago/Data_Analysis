# Sankey proof of concept for KTC
require(dplyr)
require(riverplot)
require(data.table)
require(igraph)
require(rCharts)
require(alluvial)
# Need to generate data on student outcomes

# let's start with 80 kids, since that is about the size
# of a cohort at KIPP Chicago
ktc.data<-data.table(StudentID=c(1:80), MAP_Quartile=rep(NA,80), HS1=rep(NA,80), HS2=rep(NA,80), HS3=rep(NA,80), HS4=rep(NA,80), ACT=rep(NA,80), College=rep(NA,80))

# they each need a MAP quartile

ktc.data[,MAP_Quartile:=c(rep(4, 5), rep(3, 15), rep(2,40), rep(1,20))]

hs_placement<-function(x, transition_year=FALSE){
    
  hs.types<-rev(c("NBS", 
              "Select",
              "PDS", 
              "Mag",
              "Charter",
              "Trad",
              "Alt",
              "Suburb",
              "Unknown",
              "DO"
              )
              )   
  
  hs.types<-factor(hs.types, levels=hs.types)
  if(!transition_year){
    inner<-function(y){
      switch(as.character(y),
         "4"=sample(hs.types, size=1, prob=rev(c(.4,.4,.1,.1,0,0,0,0,0,0))),
        "3"=sample(hs.types, size=1, prob=rev(c(0,.01,.2,.1,.5,.1,0,0,.05,.05))),
        "2"=sample(hs.types, size=1, prob=rev(c(0,.01,.2, 0,.5,.1,0,0,.1,.09))),
        "1"=sample(hs.types, size=1, prob=rev(c(0,0,.2, 0,.6,.1,0,0,.1,.2)))
        )
    }
    out<-lapply(x,inner)
  }

  if(transition_year) {
    inner2<-function(y){
      if(rbinom(1,2,.25)==1){
       switch(as.character(as.numeric(y)),
              "10"=sample(hs.types, size=1, prob=rev(c(0,0,.1,0,.19,.3,0,0,0,0.01))),
              "9"=sample(hs.types, size=1, prob=rev(c(0,0,0,0,.2,.7,0,0,.05,.05))),
              "8"=sample(hs.types, size=1, prob=rev(c(0,0,0,0,.29,.3,0,0,0,0.01))),
              "7"=sample(hs.types, size=1, prob=rev(c(0,0,0,0,.2,.7,0,0,.05,.05))),
              "6"=sample(hs.types, size=1, prob=rev(c(0,0,0,0,.3,.5,0,0,.1,.1))),
              "5"=sample(hs.types, size=1, prob=rev(c(0,0,.2, 0,.25,.25,0,.15,.1,.25))),
              "4"=sample(hs.types, size=1, prob=rev(c(0,0,0, 0,0,.2,.1,.1,.1,.5))),
              "3"=sample(hs.types, size=1, prob=rev(c(0,0,0, 0,.2,.2,.1,.2,.1,.2))),
              "2"=sample(hs.types, size=1, prob=rev(c(0,0,0, 0,.2,.5,0,0,.1,.2))),
              "1"=sample(hs.types, size=1, prob=rev(c(0,0,0, 0,0,.4,.2,0,.2,.2)))
              )
      }
      else {
        out<-y
      }
    }
    out<-lapply(x,inner2)
  }
  unlist(out)
}



# Create placements 
ktc.data[,HS1:=hs_placement(MAP_Quartile, FALSE)]


# create transitions
ktc.data[,HS2:=hs_placement(HS1, TRUE)]
ktc.data[,HS3:=hs_placement(HS2, TRUE)]
ktc.data[,HS4:=hs_placement(HS3, TRUE)]





ktc.data[,ACT:=MAP_Quartile+rbinom(nrow(ktc.data),1,.25)*round(runif(nrow(ktc.data),-1,1))]
ktc.data[ACT==5,ACT:=4]
ktc.data[ACT==0,ACT:=1]





# Node construction
node_cols<-colnames(ktc.data)[2:7]
e<-list()
for(i in c(3:7)){
  if(i-1==2) e<-list() 
  node1<- select(ktc.data, i-1)[[1]]
  node2<- select(ktc.data, i)[[1]]
  edge <- paste(node1,rep(i-2,length(node1)),node2,rep(i-1,length(node1)),sep="--")  
  e[[paste0("edge",i-2)]]<-edge  
}


e2<-cbind(StudentID=ktc.data$StudentID,as.data.table(e))

e2.melt<-melt(e2, id.vars = "StudentID")
n_students<-length(unique(e2.melt$StudentID))

# calculate edge weights ####
edge_weights<-group_by(e2.melt, value) %>% summarise(N=n()) %>% mutate(Pct=N/n_students)
edge_weights



# assertain unqiue nodes ####
nodes<-list()
nodes$ID<-unique(c(gsub("(.+)--(.+)--(.+)--(.+)", "\\1-\\2", edge_weights$value),gsub("(.+)--(.+)--(.+)--(.+)", "\\3-\\4", edge_weights$value)))
nodes$x <- as.integer(gsub(".+-(\\d+)", "\\1",nodes$ID))
#nodes$x <- nodes$x/max(nodes$x)
nodes$labels <-gsub("(.+)-\\d+", "\\1",nodes$ID)
nodes<-as.data.table(nodes)
#color_nodes
nodes[labels=="1", col:="red"]
nodes[labels=="2", col:="#439539"]
nodes[labels=="3", col:="#60A2D7"]
nodes[labels=="4", col:="purple"]
nodes[labels=="NBS", col:="#255694"]
nodes[labels=="PDS", col:="#A7CFEE"]
nodes[labels=="Select", col:="#BCD631"]
nodes[labels=="Mag", col:="springgreen2"]
nodes[labels=="Charter", col:="springgreen3"]
nodes[labels=="Trad", col:="#FEBC11"]
nodes[labels=="Suburb", col:="lightblue"]
nodes[labels=="Alt", col:="orange"]
nodes[labels=="DO", col:="#E27425"]
nodes[labels=="Unknown", col:="#C49A6C"]




nodes[labels=="1", y:=.125]
nodes[labels=="2", y:=.375]
nodes[labels=="3", y:=.675]
nodes[labels=="4", y:=.875]
nodes[labels=="NBS", y:=.9]
nodes[labels=="PDS", y:=.8]
nodes[labels=="Select", y:=.75]
nodes[labels=="Mag", y:=.625]
nodes[labels=="Charter", y:=.5]
nodes[labels=="Trad", y:=.375]
nodes[labels=="Suburb", y:=.3]
nodes[labels=="Alt", y:=.25]
nodes[labels=="DO", y:=.2]
nodes[labels=="Unknown", y:=.125]

lab<-c("1","2", "3", "4", "NBS", "PDS", "Select", "Mag", "Charter", "Trad", "Suburb", "Alt", "DO", "Unknown")
nodes[,labels:=factor(labels, levels=lab)]

#nodes[,y:=NULL]

#create edge df ####
edges<-data.table(edge_weights)
edges[,N1:=c(gsub("(.+)--(.+)--(.+)--(.+)", "\\1-\\2", value))]
edges[,N2:=c(gsub("(.+)--(.+)--(.+)--(.+)", "\\3-\\4", value))]
edges[,value:=NULL]
setnames(edges, "Pct", "Value")


r<-makeRiver(nodes=data.frame(nodes), edges=data.frame(edges))

plot(r, srt=0)


edges[,N2:=paste(gsub("(.+)--(.+)--(.+)", 
                  "\\2", 
                  value),
             as.integer(
               gsub("(.+)--(.+)--(.+)", 
                    "\\3",value)
               )+1,
             sep="-"
             )
      ]




# Alluvial method ####




ktc.alluvial<-ktc.data[,.N, by=list(MAP_Quartile, HS1, HS2, HS3, HS4, ACT)]

ktc.alluvial.2<-ktc.data[,list(StudentID, MAP_Quartile, HS1, HS2, HS3, HS4, ACT, N=1)]

ktc.alluvial[]


alluvial(ktc.alluvial[,list(MAP_Quartile, HS1, HS2, HS3, HS4, ACT)], 
         freq=ktc.alluvial$N, 
         border=NA,
         col=ifelse(ktc.alluvial$HS1=="NBS"|ktc.alluvial$HS1=="PDS", 
                    "red", 
                    "gray"
                    )
         )



# Charter school in year 1

alluvial(ktc.alluvial[,list(MAP_Quartile, HS1, HS2, HS3, HS4, ACT)], 
         freq=ktc.alluvial$N, 
         #border=NA,
         col=ifelse(ktc.alluvial$HS1=="Charter",
                    "springgreen", 
                    "gray"
         )
)


# with student IDs
alluvial(ktc.alluvial.2[,list(StudentID=factor(StudentID, levels=rev(StudentID)), MAP_Quartile, HS1, HS2, HS3, HS4, ACT)], 
         freq=ktc.alluvial.2$N, 
         border=NA,
         col=ifelse(ktc.alluvial.2$HS1=="DO"|
                      ktc.alluvial.2$HS2=="DO"|
                      ktc.alluvial.2$HS3=="DO"|
                      ktc.alluvial.2$HS4=="DO"
                    , "orange", "gray"))


# Parallel sets with rcharts ####
require(RCurl)
require(rCharts)

f <- getURL('https://raw.githubusercontent.com/benjh33/rcharts_plugins/master/parallelsets/titanic_agg.csv')
d <- read.table(text=f, header=T, sep = ',')
glimpse(d)

# this creates a new S4 class called ParallelSets that inherits rCharts (I think)
ParallelSets = setRefClass('ParallelSets', contains = 'rCharts', methods = list(
  initialize = function(){
    callSuper()
    LIB <<- get_lib("http://mostlyconjecture.com/rcharts_plugins/parallelsets")
    lib <<- "parallelsets"
    templates$script <<- '
    <script type="text/javascript">
    function draw{{chartId}}(){
    var params = {{{ chartParams }}}
    var chart = {{{ parsets }}}
    
    d3.select("#{{chartId}}").append("svg")
    .datum({{{ data }}})
    .call(chart)
    return chart;
    };
    
    $(document).ready(function(){
    draw{{chartId}}()
    });
    
    </script>'
  },
  getPayload = function(chartId){
    skip = c('id', 'dom')
    parsets = toChain(params[!(names(params) %in% c(skip, 'data'))], "d3.parsets()")
    chartParams = RJSONIO:::toJSON(params[skip])
    list(parsets = parsets, chartParams = chartParams, data=toJSONArray(params[['data']]),
         chartId = chartId, lib = basename(lib), liburl = LIB$url
    )
  }
))

chart <- ParallelSets$new()
str(chart)

ktc.pp<-select(ktc.data, MAP_Quartile, HS1, HS2, HS3, HS4, ACT)

chart$set(data = ktc.pp,
          dimensions = names(ktc.pp),
          height= 800,
          width=650,
          dom="parsets", # human-readable identifier for css tweaking later 
          tension=0.4 # controls curve of bands
)

chart

# Mike Bostok's sankey plugin vai rcharts ####
# get the data in source, target, value format
ktc.bostock<-rbind(data.frame(source=paste("MAP", ktc.data$MAP_Quartile), target=paste("Y1", ktc.data$HS1)),
      data.frame(source=paste("Y1", ktc.data$HS1), target=paste("Y2",ktc.data$HS2)),
      data.frame(source=paste("Y2",ktc.data$HS2), target=paste("Y3",ktc.data$HS3)),
      data.frame(source=paste("Y3",ktc.data$HS3), target=paste("Y4",ktc.data$HS4)),
      data.frame(source=paste("Y4",ktc.data$HS4), target=paste("ACT", ktc.data$ACT))
      )

ktc.bostsock.sum<-group_by(ktc.bostock, source, target) %>% summarise(value=n()/nrow(ktc.bostock))

bostockPlot<-rCharts$new()
bostockPlot$setLib('http://timelyportfolio.github.io/rCharts_d3_sankey/libraries/widgets/d3_sankey')

bostockPlot$set(
  data = ktc.bostsock.sum,
  nodeWidth = 15,
  nodePadding = 10,
  layout = 32,
  width = 750,
  height = 500,
  labelFormat = ".1%"
)

bostockPlot

# Rewrite of alluvial in ggplot ####



ktc.alluvial.melt<-melt(ktc.alluvial.2,id.vars = c("StudentID", "N"))

ktc.alluvial.melt
group_by(ktc.alluvial.melt, )

ggplot(filter(ktc.alluvial.melt, variable!=N), aes(x=variable, y=StudentID)) +
geom_point(aes(color=value)) +geom_line()

n.students<-10
n.cats<-5
n.perf_levels<-7

students<-1:n.students
cats<-1:n.cats
perf_levels<-1:n.perf_levels

d<-as.data.table(expand.grid(StudentID=students, Category=cats))

d[,Performance:=sample(perf_levels,.N,replace = TRUE), by=list(StudentID)]

d[,Rank:=rank(Performance, ties.method = "first"), by=Category]

d[,y:=Rank/max(Rank), by=Category]


f <- function(x, sid, n=10){
  x<-filter(x, StudentID==sid)
  d<-data.frame(spline(x$Category, x$y, n=nrow(x)*n,method="fmm" )) %>%
    mutate(StudentID=sid)
  d
}


f2 <- function(x, sid, shape=1){
  x<-filter(x, StudentID==sid)
  spl<-xsplinePoints(xsplineGrob(x=x$Category, y=x$y, open=TRUE, shape=shape, default.units = "inches", repEnds = T))
  d<-data.frame(x=as.numeric(spl$x), y=as.numeric(spl$y)) %>%
    mutate(StudentID=sid
           #, x=x/max(x)*5, y=y/max(y)
           )
  d
}



add_padding<-function(x, padding=.1){
  x.out<-c(x-(2*padding), x-padding, x , x+padding, x+(2*padding))
  sort(x.out)
}


f2 <- function(.data, sid, shape=1, ...){
  .data<-filter(.data, StudentID==sid)
  perfs<-unique(.data$Performance)
  n.perf<-length(perfs)
  x<-add_padding(.data$Category, ...)
  y<-rep(.data$y, each=5)
  shape=rep(c(1,0,0,0,1), times=5)
  spl<-xsplinePoints(xsplineGrob(x=x, y=y, open=TRUE, shape=shape, default.units = "inches", repEnds = T))
  d<-data.frame(x=as.numeric(spl$x), y=as.numeric(spl$y)) %>%
    mutate(StudentID=sid)
  d
}




#d.splines<-rbindlist(lapply(X = unique(d$StudentID), FUN = f, x=d, n=10))

d.splines2<-rbindlist(lapply(X = unique(d$StudentID), FUN = f2, .data=d, shape=-.25))

#d.converted <- mutate(d, x=unit(x,"npc"), y=unit(y,"npc"))
#setnames(d.splines, "x", "Category")

setnames(d.splines2, "x", "Category")

d.splines2$ymax<-d.splines2$y+.05
d.splines2$ymin<-d.splines2$y-.05

ggplot(data=d.splines2, aes(x=Category, y=y)) + 
  geom_point(data=d, aes(color=StudentID)) +
  geom_line(aes(group=StudentID, color=StudentID)) + 
  geom_ribbon(aes(x=Category, 
                  ymax=ymax, 
                  ymin=ymin,
                  group=StudentID,
                  fill=StudentID), alpha=.2) 





