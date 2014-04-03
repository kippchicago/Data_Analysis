# Load packages ####
require(ggplot2)
require(maptools)
require(rgeos)
require(rgdal)
require(ggmap)
require(RJDBC)
require(data.table)

gpclibPermit()

setwd("~/Dropbox/Consulting/KIPP Ascend/Data Analysis/GIS")


# get student data from PowerSchool ####
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


# get shape files ####
chi_boundaries.rg<-readOGR("shapefiles/City_Boundary", "City_Boundary")

#get zip codes
zip_codes<-readOGR("shapefiles/ZipCodes", "ZipCodes")
# prep zipcodes for ggplot
zip_codes@data$id <- rownames(zip_codes@data)
zips <- fortify(zip_codes, region="id")
zips.df<-merge(zips, zip_codes, by="id")
zips.dt<-data.table(zips.df[order(zips.df$id, zips.df$order),])

# get major streets
major_streets <- readOGR("shapefiles/Major_Streets/", "Major_Streets")

major_streets@data$id <- rownames(major_streets@data)
streets <- fortify(major_streets, region = "id")
streets.df <- merge(streets, major_streets, by="id")

streets.dt<-data.table(streets.df[order(streets.df$id, streets.df$order),])
streets.data.dt<-data.table(major_streets@data)

setkey(streets.dt, "id")
setkey(streets.data.dt, "id")

streets.dt<-streets.dt[streets.data.dt]

# CPS Schools ####
cps_schools <- readOGR("shapefiles/School_Grounds/", "School_Grounds")

cps_schools@data$match_col<-with(cps_schools@data, paste(SCHOOL_NAM, PRIMARY_AD))

CPS.Schools[,match_col:=paste(INCS_School_Name, Street_Address)]
CPS.Schools[,School_Type:=as.character(INCS_School_Type)]

school_type<-unlist(lapply(as.list(cps_schools$match_col), 
                           best_match, 
                           .data=as.data.frame(CPS.Schools[INCS_School_Type=="Charter"]), 
                           match_column=match_col, 
                           output_column=School_Type, 
                           confidence=.6))


cps_schools@data$id <- rownames(cps_schools@data)
cps.df <- fortify(cps_schools, region = "id")
cps_schools.df <- merge(cps.df, cps_schools, by="id")

#cps_schools.data.dt<-data.table(cps_schools@data)
cps.dt<-data.table(cps_schools.df)
cps.dt[,School_Type2:=ifelse(School_Type=="Charter", "Charter", "Non-Charter")]
cps.dt[,School_Type2_col:=ifelse(School_Type2=="Charter", "black", "#C49A6C")]

charter_lonlat<-as.matrix(CPS.Schools[INCS_School_Type=="Charter",list(Long, Lat)])
charter_reprojected<-project(charter_lonlat, proj=p4)
CPS.Schools[INCS_School_Type=="Charter", long:=charter_reprojected[,1]]
CPS.Schools[INCS_School_Type=="Charter", lat:=charter_reprojected[,2]]

# 




# geocode addresses ####
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


# bus stops
bus_stops.geocoded <- geocode(bus.stops[,paste(Stop_address, City_State)])
bus_stops.reprojected<-project(as.matrix(bus_stops.geocoded), proj=p4)
bus.stops[,long:=bus_stops.reprojected[,1]]
bus.stops[,lat:=bus_stops.reprojected[,2]]

# fix "Shine 3" to equal "Shine-3"
bus.stops[Route=="Shine 3", Route:="Shine-3"]

# refactor Route names
bus.stops[,Route:=factor(as.character(Route), levels=c("Tenacity-1", "Empower-2",  "Shine-3", "Engage-4"))]




cols<-gg_color_hue(3)

students.clipped[School=="KAMS",School_col:=cols[1]]
students.clipped[School=="KCCP",School_col:=cols[2]]
students.clipped[School=="KBCP",School_col:=cols[3]]

zips.dt[School=="KAMS",School_col:=cols[1]]
zips.dt[School=="KCCP",School_col:=cols[2]]
zips.dt[School=="KBCP",School_col:=cols[3]]

school_location[School=="KAMS",School_col:=cols[1]]
school_location[School=="KCCP",School_col:=cols[2]]
school_location[School=="KBCP",School_col:=cols[3]]

p <- ggplot(data=students.clipped[School!="KAP"], aes(x=long, y=lat)) + 
  geom_segment(data=major_streets_lines, aes(x=slon, y=slat, xend=elon, yend=elat), color="lightgray", lineend = "round", alpha=.5) +
  geom_polygon(data=zips.dt, aes(x=long, y=lat, group=group), color="darkgray", fill=NA) + 
  geom_polygon(data=zips.dt[!is.na(School)], aes(x=long, y=lat, group=group, fill=School), alpha=.1, color=NA) + 
  geom_point(aes(x=long, y=lat, color=School), alpha=.6, size=1.75) + 
  geom_point(data=school_location, aes(x=long, y=lat, fill=School), color="black", shape=23, size=6, alpha=.5) +
  geom_text(data=school_location, aes(x=long, y=lat, label=School), hjust=0, vjust=0, alpha=.5) +
  geom_polygon(data=cps.dt, aes(x=long, y=lat, group=group, fill="Non-KIPP School", color="Non-KIPP School")) +
  scale_fill_manual(values = c(cols, '#706f6c')) + 
  #scale_color_identity() +
  #facet_wrap(~GRADE_LEVEL) + 
  theme_minimal()



p.buses <- p + 
  geom_point(data=CPS.Schools[INCS_School_Type=="Charter" & lat>1200000], 
             aes(long, lat, fill="Charter School", color="Charter School"), 
             shape=8, size=2) +  
  geom_point(data=bus.stops, aes(x=long, y=lat, shape=Route), alpha=.5, color="purple", fill="purple", size=3) +
  scale_fill_manual(values=c("#FEBC11",cols, "#706f6c")) +
  scale_color_manual(values=c("#FEBC11",cols, "#706f6c")) +
  scale_shape_manual("KAMS Bus Routes", values=c(7, 9, 10, 12)) +
  ggtitle("KIPP Chi: Students by School\nwith Busing ZIP Codes Highlighted")



p.less <- ggplot(data=students.clipped[School!="KAP"], aes(x=long, y=lat)) + 
  geom_polygon(data=zips.dt, aes(x=long, y=lat, group=group), color="darkgray", fill=NA) + 
  geom_polygon(data=zips.dt[!is.na(School)], aes(x=long, y=lat, group=group, color=School, fill=School), alpha=.1) + 
  geom_point(aes(x=long, y=lat, color=School), alpha=.6, size=1.75) + 
  geom_point(data=school_location, aes(x=long, y=lat, fill=School), color="black", shape=23, size=4, alpha=.5) +
  geom_text(data=school_location, aes(x=long, y=lat, label=School), hjust=0, vjust=0, alpha=.5) +
  theme_minimal()


pdf("graphs/busing_map_with_schools.pdf", height=11, width=8.5)
inset_zoom(p.buses, 
           zoom=2, 
           x.expand=c(1135000, 1165000), 
           y.expand=c(1885000, 1910000), 
           j.expand="bl", 
           inset.title="Vicinity of KAMS & KCCP")

p.less + facet_wrap(~GRADE_LEVEL) + ggtitle("KIPP Chi: Students by Grade & School\nwith Busing ZIP Codes Highlighted")
dev.off()
