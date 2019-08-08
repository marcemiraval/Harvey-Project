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
setwd ("Chapter1/Colorado/ColoradoState")

# Importing and setting polygons for Colorado Counties
coloradoPol <- readRDS(file = "Outputs/colorado_state_poly.rds") # Colorado State Polygon with counties subdivision


# FLASH #############

# Setting projection to Lambert Conformal Conic for reports
coordinates(tidy_flash)<-~x+y
proj4string(tidy_flash) <- CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 
                               +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
tidy_flash<- data.frame(x=coordinates(tidy_flash)[,1], y=coordinates(tidy_flash)[,2], 
                        tidy_flash@data) 

FlashInColorado <- ggplot(data = coloradoPol) +
  geom_polygon(aes(x=long, y=lat, group=group), fill="grey20", colour="grey35", alpha=1)+
  geom_point(data = tidy_flash, aes(x=x, y=y), color="#ae017e", alpha = 1, size=0.5) +
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

FlashInColorado

ggsave(filename = "FlashColMap", plot = FlashInColorado, device = "png", 
       path = "Outputs", width=5, height=6, units="in", dpi = 300, bg = "Black")


# RAIN #############

# Setting projection to Lambert Conformal Conic for reports
coordinates(tidy_rain)<-~x+y
proj4string(tidy_rain) <- CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 
                               +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
Rain_co<- data.frame(x=coordinates(tidy_rain)[,1], y=coordinates(tidy_rain)[,2], 
                     tidy_rain@data) 

RainInColorado <- ggplot(data = coloradoPol) +
  geom_polygon(aes(x=long, y=lat, group=group), fill="grey20", colour="grey35", alpha=1)+
  geom_point(data = Rain_co, aes(x=x, y=y), color="#9ebcda", alpha = 1, size=0.5) +
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

RainInColorado

ggsave(filename = "RainColMap", plot = RainInColorado, device = "png", 
       path = "Outputs", width=5, height=6, units="in", dpi = 300, bg = "Black")


# SNOW #############

# Setting projection to Lambert Conformal Conic for reports
coordinates(tidy_snow)<-~x+y
proj4string(tidy_snow) <- CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 
                              +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
Snow_co<- data.frame(x=coordinates(tidy_snow)[,1], y=coordinates(tidy_snow)[,2], 
                     tidy_snow@data) 

SnowInColorado <- ggplot(data = coloradoPol) +
  geom_polygon(aes(x=long, y=lat, group=group), fill="grey20", colour="grey35", alpha=1)+
  geom_point(data = Snow_co, aes(x=x, y=y), color="#f7fbff", alpha = 1, size=0.5) +
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

SnowInColorado

ggsave(filename = "SnowColMap", plot = SnowInColorado, device = "png", 
       path = "Outputs", width=5, height=6, units="in", dpi = 300, bg = "Black")


# THUNDERSTORM #############

# Setting projection to Lambert Conformal Conic for reports
coordinates(tidy_thunderstorm)<-~x+y
proj4string(tidy_thunderstorm) <- CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 
                              +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
Thunderstorm_co<- data.frame(x=coordinates(tidy_thunderstorm)[,1], y=coordinates(tidy_thunderstorm)[,2], 
                     tidy_thunderstorm@data) 

ThunderstormInColorado <- ggplot(data = coloradoPol) +
  geom_polygon(aes(x=long, y=lat, group=group), fill="grey20", colour="grey35", alpha=1)+
  geom_point(data = Thunderstorm_co, aes(x=x, y=y), color="#a6d96a", alpha = 1, size=0.5) +
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

ThunderstormInColorado

ggsave(filename = "ThunderstormColMap", plot = ThunderstormInColorado, device = "png", 
       path = "Outputs", width=5, height=6, units="in", dpi = 300, bg = "Black")


# WATER #############

# Setting projection to Lambert Conformal Conic for reports
coordinates(tidy_water)<-~x+y
proj4string(tidy_water) <- CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 
                                      +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
Water_co<- data.frame(x=coordinates(tidy_water)[,1], y=coordinates(tidy_water)[,2], 
                             tidy_water@data) 

WaterInColorado <- ggplot(data = coloradoPol) +
  geom_polygon(aes(x=long, y=lat, group=group), fill="grey20", colour="grey35", alpha=1)+
  geom_point(data = Water_co, aes(x=x, y=y), color="#253494", alpha = 1, size=0.5) +
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

WaterInColorado

ggsave(filename = "WaterColMap", plot = WaterInColorado, device = "png", 
       path = "Outputs", width=5, height=6, units="in", dpi = 300, bg = "Black")

