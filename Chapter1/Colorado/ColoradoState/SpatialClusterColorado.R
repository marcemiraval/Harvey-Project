
# http://rpubs.com/RobinLovelace/14465

#install.packages(c("rgdal", "sp"))
library(rgdal)
library(sp)# add spatial package to load the S4 spatial objects
library(spatstat)  # to calculate field of point density
library(maptools)  # to convert to point pattern
library(GISTools)

# Defining working directory
setwd ("/home/marcela/Coding/EWE-reporting/Colorado")
US <- readOGR(dsn = "cb_2016_us_state_20m/cb_2016_us_state_20m.shp")

# Setting projection to Lambert Conformal Conic
us.prjd <- spTransform(US, CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 
                                 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")) 
us.prjd <- subset(us.prjd, (NAME != "Puerto Rico" & NAME != "Alaska" & NAME != "Hawaii"))

Reports <- read.csv(file="Colorado_Clean.csv", header=TRUE, sep=",")
coordinates(Reports)<-~longitude+latitude
proj4string(Reports) <- CRS("+init=epsg:4326")
reports.prjd <- spTransform(Reports, CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 
                                 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")) 

sSp <- as(SpatialPoints(reports.prjd), "ppp")  # convert points to pp class
Dens <- density(sSp, adjust = 0.2)  # create density object
class(Dens)  # just for interest: it's got it's of pixel image class
plot(Dens)
contour(density(sSp, adjust = 0.2), nlevels = 4)  # plot as contours - this is where we're heading

bndry<-as.owin(us.prjd)
reports.pp<-ppp(coordinates(reports.prjd)[,1], coordinates(reports.prjd)[,2], window=bndry) #unmarked point pattern data
r.pp<-unique(reports.pp) #removing duplicated points
r.pp<-rescale(r.pp,1000) #Convert units to kilometers
layout(matrix(1:4,2,2))
par(mai=rep(0.5,4))
plot(density.ppp(r.pp,5))
plot(density.ppp(r.pp,35))
plot(density.ppp(r.pp,50))
plot(density.ppp(r.pp,75))


#### MAPPING COLORADO REPORTS WITHIN THE US ######################################
ggplot(us.prjd) +geom_polygon(aes(x=long, y=lat, group=group), fill="grey20", 
                         colour="grey35", alpha=1) +
  labs(x="", y="", title="") + geom_point(data=as.data.frame(coordinates(reports.prjd)), 
                                          aes(x=coordinates(reports.prjd)[,1], y=coordinates(reports.prjd)[,2], color="cyan"),
                                          alpha = 1 / 8, size=0.05)+
  theme(legend.position = "none") +
  coord_equal(ratio=1) +
  theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(),# get rid of x and y ticks
        axis.text.y = element_blank(),
        axis.text.x = element_blank(), legend.position = "none", 
        panel.background = element_rect(fill = "grey12", colour = NA),
        panel.grid.major = element_line(colour = "grey18"),
        panel.grid.minor = element_line(colour = "grey18", size = 0.25)
  )

# Probably won't need thsi part because there are no reports outside US
# inside.area <- !is.na(over(Reports, as(us.prjd, "SpatialPolygons")))
# colorado.US<-Reports[inside.area, ]
# Colorado_US<- data.frame(x=coordinates(colorado.US)[,1], y=coordinates(colorado.US)[,2],
#                      colorado.US@data) 

