geocodeBatch <- function(address, bounding_box) {
  #URL for batch requests
  URL=paste("http://open.mapquestapi.com/geocoding/v1/batch?key=", "Fmjtd%7Cluub2huanl%2C20%3Do5-9uzwdz", 
            "&location=", paste(address,collapse="&location="),sep = "") 
  
  URL <- gsub(" ", "+", URL)
  data<-RCurl::getURL(URL)
  data <- rjson::fromJSON(data)
  
  p<-sapply(data$results,function(x){
    if(length(x$locations)==0){
      c(NA,NA)
    } else{
      c(x$locations[[1]]$displayLatLng$lat, x$locations[[1]]$displayLatLng$lng)   
    }})
  
  out_list<-t(p)
  out<-data.frame(lat=out_list[,1], long=out_list[,2])  
}
