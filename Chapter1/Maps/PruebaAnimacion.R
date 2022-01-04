library(rgdal)
library(sp)
library(GISTools)
library(maps)
library(maptools)
library(ggplot2)
library(ggmap)
library(lattice)
library(ggthemes)
library(gganimate)


# Data to create basemap

# Defining working directory
setwd ("/home/marcela/Coding/EWE-reporting/Colorado/ColoradoState")
US <- readOGR(dsn = "cb_2016_us_county_20m/cb_2016_us_county_20m.shp")

# Setting projection to Lambert Conformal Conic
us.prjd <- spTransform(US, CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 
                                 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")) 

# Filtering Out polygon for Colorado
coloradoPol <- subset(us.prjd, (STATEFP =="08")) # 

coloradoR <- read.csv("/home/marcela/Coding/EWE-reporting/Colorado/Colorado_Clean.csv", header = TRUE, sep = ",")
coordinates(coloradoR)<-~longitude+latitude  #Makes coloradoR a spatial object
proj4string(coloradoR) <- CRS("+init=epsg:4326") #set datum |4326 is the epsg identifier of wgs 84

coloradoR <- spTransform(coloradoR, CRSobj = CRS(proj4string(coloradoPol)))

inside.area <- !is.na(over(coloradoR, as(coloradoPol, "SpatialPolygons")))

colorado.inside<-coloradoR[inside.area, ]
ColoradoR_co<- data.frame(x=coordinates(colorado.inside)[,1], y=coordinates(colorado.inside)[,2],
                     colorado.inside@data) 

ColoradoR_co$date = as.Date(ColoradoR_co$created_at) ####

#ColoradoR_co$date = as.factor(ColoradoR_co$date)

# write.csv(ColoradoR_co, "Colorado_Subset.csv", row.names=FALSE)

ReportsInColorado <- ggplot(ColoradoR_co, aes(frame = date)) +
  geom_point(aes(x=x, y=y),color="Purple", size=0.2)

ReportsInColorado <- ggplot(data = ColoradoR_co, aes(x=x, y=y, frame = date)) +
  geom_polygon(data = coloradoPol, aes(x=long, y=lat, group=group), fill="grey20", colour="grey35", alpha=1)+
  geom_point(color="Purple", alpha = 1 / 8, size=0.5) +
  theme(legend.position = "none") +
  coord_equal(ratio=1) +
  labs(x="", y="", title="")+
  coord_equal(ratio=1)

ReportsInColorado
                            
gganimate(ReportsInColorado,ani.width=1200, ani.height=800,interval=0.2,"coloradoMarce.gif")  ####


ReportsInColorado

ggsave(filename = "ColoradoColMap", plot = ReportsInColorado, device = "png", 
       path = "Outputs", width=5, height=6, units="in", dpi = 300, bg = "Black")
