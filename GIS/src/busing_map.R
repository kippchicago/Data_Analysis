require(ggplot2)
require(maptools)
require(rgeos)
require(rgdal)
require(ggmap)
require(RJDBC)
require(data.table)


# get student data from PowerSChool
drvr <- JDBC("oracle.jdbc.driver.OracleDriver", "/Users/chaid/Dropbox/JDBC Drivers/ojdbc6.jar","") # define driver

pspw <- as.list(read.dcf("config/ps.dcf", all=TRUE)) #read DCF with configuration settings

pscon <- dbConnect(drvr,pspw$SERVER,pspw$UID,pspw$PWD) # connect to server

qry<-paste("SELECT student_number as StudentID,
                   first_name,
                   last_name,
                   grade_level,
                   street,
                   city, 
                   state,
                   zip,
                   schoolid
           FROM students 
           WHERE enroll_status=0")

students<-data.table(dbGetQuery(pscon, qry))

students[SCHOOLID==78102,School:="KAP"]
students[SCHOOLID==7810,School:="KAMS"]
students[SCHOOLID==400146,School:="KCCP"]
students[SCHOOLID==400163,School:="KBCP"]

students[,Address:=paste(gsub("(.+),(.+)", "\\1", students$STREET), ZIP)]


#get shape files
chi_boundaries.rg<-readOGR("shapefiles/City_Boundary", "City_Boundary")

zip_codes<-readOGR("shapefiles/ZipCodes", "ZipCodes")


zip_codes@data$id <- rownames(zip_codes@data)

zips <- fortify(zip_codes, region="id")

zips.df<-merge(zips, zip_codes, by="id")

zips.dt<-data.table(zips.df[order(zips.df$id, zips.df$order),])

#geocode addresses
addr_geocoded<-geocode(students$Address)

# reproject_addresses
# get projection from shapefile
p4<-proj4string(zip_codes)

#reproject
addr_reporjected<-project(as.matrix(addr_geocoded), proj = p4)

#write to locations students
students[,long:=addr_reporjected[,1]]
students[,lat:=addr_reporjected[,2]]

#clip wrong adresses from students
students.clipped<-students[long >= 1100000 & long<=1220000 & lat>=1800000 & lat<= 2000000]


zips.dt[ZIP %in% c(60623, 60608, 60607, 60605, 60616), 
        School:="KAMS"]
zips.dt[ZIP %in% c(60641, 60618, 60707, 60639, 60647, 
                   60651, 60622, 60644), 
        School:="KCCP"]
zips.dt[ZIP %in% c(60638, 60632, 60609, 60653, 60615, 
                   60629, 60636, 60621, 60637, 60649, 
                   60652, 60620, 60619, 60617, 60633, 
                   60655, 60643, 60628, 60827), 
        School:="KBCP"]

school_location<-data.table(School=c("KAMS", "KCCP", "KBCP"), Address=c("1616 S Avers Ave 60623", 
                                                                          "4818 W Ohio St 60644",
                                                                          "5515 S Lowe Ave 60621"))

schools.geocoded<-geocode(school_location$Address)
schools.reprojected<-project(as.matrix(schools.geocoded), proj = p4)

school_location[,long:=schools.reprojected[,1]]
school_location[,lat:=schools.reprojected[,2]]


p <- ggplot(data=students.clipped[School!="KAP"], aes(x=long, y=lat)) + 
  geom_polygon(data=zips.dt, aes(x=long, y=lat, group=group), color="gray", fill=NA) + 
  geom_polygon(data=zips.dt[!is.na(School)], aes(x=long, y=lat, group=group, color=School, fill=School), alpha=.1) + 
  geom_point(aes(x=long, y=lat, color=School), alpha=.6, size=3) + 
  geom_point(data=school_location, aes(x=long, y=lat, fill=School), color="black", shape=23, size=6, alpha=.5) +
  geom_text(data=school_location, aes(x=long, y=lat, label=School), hjust=0, vjust=0, alpha=.5) +
  #facet_wrap(~GRADE_LEVEL) + 
  theme_minimal()

pdf("graphs/busing_map.pdf", height=11, width=8.5)
p + ggtitle("KIPP Chi: Students by School\nwith Busing ZIP Codes Highlighted")

p + facet_wrap(~GRADE_LEVEL) + ggtitle("KIPP Chi: Students by Grade & School\nwith Busing ZIP Codes Highlighted")
dev.off()