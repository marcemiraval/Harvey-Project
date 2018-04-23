library(rgdal)
library(sp)
library(GISTools)
library(maps)
library(maptools)
library(ggplot2)
library(ggmap)
library(lattice)
library(ggthemes)

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

write.csv(ColoradoR_co, "Colorado_Subset.csv", row.names=FALSE)

ReportsInColorado <- ggplot(data = coloradoPol) +
  geom_polygon(aes(x=long, y=lat, group=group), fill="grey20", colour="grey35", alpha=1)+
  geom_point(data = ColoradoR_co, aes(x=x, y=y), color="Purple", alpha = 1 / 8, size=0.5) +
  theme(legend.position = "none") +
  coord_equal(ratio=1) +
  labs(x="", y="", title="")+
  coord_equal(ratio=1) +
  annotate(geom = "text", 
           x=getSpPPolygonsLabptSlots(coloradoPol)[,1], 
           y=getSpPPolygonsLabptSlots(coloradoPol)[,2],
           label = coloradoPol$NAME,
           size = 1.2,
           color = "white") +
  theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(),# get rid of x and y ticks
        axis.text.y = element_blank(),
        axis.text.x = element_blank(), legend.position = "none", 
        panel.background = element_rect(fill = "grey12", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )

ReportsInColorado

ggsave(filename = "ColoradoColMap", plot = ReportsInColorado, device = "png", 
       path = "Outputs", width=5, height=6, units="in", dpi = 300, bg = "Black")
