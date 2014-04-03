# Sankey proof of concept for KTC
require(dplyr)
require(riverplot)
require(data.table)

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
edge_weights<-summarise(group_by(e2.melt, value),Pct=n()/n_students)



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
         col=ifelse(ktc.alluvial$HS1=="DO"|
                      ktc.alluvial$HS2=="DO"|
                      ktc.alluvial$HS3=="DO"|
                      ktc.alluvial$HS4=="DO", 
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
         border=ifelse(ktc.alluvial.2$StudentID=="10", "seagreen", "gray"),
         col=ifelse(ktc.alluvial.2$StudentID=="10", "seagreen", "gray"))





