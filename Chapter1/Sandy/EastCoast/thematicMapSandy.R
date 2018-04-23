library(rgdal)
library(sp)
library(GISTools)
library(maps)
library(maptools)
library(ggplot2)
library(ggmap)
library(lattice)
library(ggthemes)
library(dplyr)


# Defining working directory
setwd ("/home/marcela/Coding/EWE-reporting/Sandy/EastCoast")

# Data to create basemap
data(tornados) 

proj4string(us_states)

# Filtering states affected by Sandy according to SHELDUS database in order to create AoI
index <- us_states$STATE_NAME == "Maryland" | us_states$STATE_NAME == "Delaware"| 
  us_states$STATE_NAME == "New Jersey" | us_states$STATE_NAME == "New York"|
  us_states$STATE_NAME == "Connecticut" | us_states$STATE_NAME == "Massachusetts"|
  us_states$STATE_NAME == "Rhode Island"| us_states$STATE_NAME == "North Carolina"|
  us_states$STATE_NAME == "Virginia" | us_states$STATE_NAME ==  "West Virginia"|
  us_states$STATE_NAME == "Ohio"| us_states$STATE_NAME ==  "Pennsylvania"|
  us_states$STATE_NAME == "New Hampshire"   

# Defining area of interest --> states affected by Sandy
AoI <- us_states[index,]

# proj4string(AoI.prjd)
# AoI.prjd <- spTransform(AoI, CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 + x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")) 


# FLOOD #############

# Setting projection to Lambert Conformal Conic for reports
coordinates(tidy_flood)<-~x+y
tidy_flood<- data.frame(x=coordinates(tidy_flood)[,1], y=coordinates(tidy_flood)[,2], 
                        tidy_flood@data) 

FloodSandy <- ggplot(data = AoI) +
  geom_polygon(aes(x=long, y=lat, group=group), fill="grey20", colour="grey35", alpha=1) + 
  geom_point(data = tidy_flood, aes(x=x, y=y), color="#ffff99", alpha = 0.5, size=0.15) +
  coord_map("lambert", lat0=20, lat=50)+
  theme(legend.position = "none") +
  coord_equal(ratio=1) +
  labs(x="", y="", title="")+
  coord_equal(ratio=1) + 
  annotate(geom = "text", 
           x = getSpPPolygonsLabptSlots(AoI)[,1],
           y = getSpPPolygonsLabptSlots(AoI)[,2],
           label = AoI$STATE_NAME,
           size = 1.2,
           color = "white") + 
  theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(),# get rid of x and y ticks
        axis.text.y = element_blank(),
        axis.text.x = element_blank(), legend.position = "none", 
        panel.background = element_rect(fill = "grey12", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )


FloodSandy

ggsave(filename = "FloodSandyMap", plot = FloodSandy, device = "png", 
       path = "Outputs", width=5, height=6, units="in", dpi = 300, bg = "Black")


# POWER #############

# Setting projection to Lambert Conformal Conic for reports
coordinates(tidy_power)<-~x+y
tidy_power <- data.frame(x=coordinates(tidy_power)[,1], y=coordinates(tidy_power)[,2], 
                        tidy_power@data) 

PowerSandy <- ggplot(data = AoI) +
  geom_polygon(aes(x=long, y=lat, group=group), fill="grey20", colour="grey35", alpha=1) + 
  geom_point(data = tidy_power, aes(x=x, y=y), color="#e78ac3", alpha = 0.5, size=0.15) +
  coord_map("lambert", lat0=20, lat=50)+
  theme(legend.position = "none") +
  coord_equal(ratio=1) +
  labs(x="", y="", title="")+
  coord_equal(ratio=1) + 
  annotate(geom = "text", 
           x = getSpPPolygonsLabptSlots(AoI)[,1],
           y = getSpPPolygonsLabptSlots(AoI)[,2],
           label = AoI$STATE_NAME,
           size = 1.2,
           color = "white") + 
  theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(),# get rid of x and y ticks
        axis.text.y = element_blank(),
        axis.text.x = element_blank(), legend.position = "none", 
        panel.background = element_rect(fill = "grey12", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )


PowerSandy

ggsave(filename = "PowerSandyMap", plot = PowerSandy, device = "png", 
       path = "Outputs", width=5, height=6, units="in", dpi = 300, bg = "Black")



# RAIN #############

# Setting projection to Lambert Conformal Conic for reports
coordinates(tidy_rain)<-~x+y
tidy_rain <- data.frame(x=coordinates(tidy_rain)[,1], y=coordinates(tidy_rain)[,2], 
                         tidy_rain@data) 

RainSandy <- ggplot(data = AoI) +
  geom_polygon(aes(x=long, y=lat, group=group), fill="grey20", colour="grey35", alpha=1) + 
  geom_point(data = tidy_rain, aes(x=x, y=y), color="#fdb462", alpha = 0.5, size=0.15) +
  coord_map("lambert", lat0=20, lat=50)+
  theme(legend.position = "none") +
  coord_equal(ratio=1) +
  labs(x="", y="", title="")+
  coord_equal(ratio=1) + 
  annotate(geom = "text", 
           x = getSpPPolygonsLabptSlots(AoI)[,1],
           y = getSpPPolygonsLabptSlots(AoI)[,2],
           label = AoI$STATE_NAME,
           size = 1.2,
           color = "white") + 
  theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(),# get rid of x and y ticks
        axis.text.y = element_blank(),
        axis.text.x = element_blank(), legend.position = "none", 
        panel.background = element_rect(fill = "grey12", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )


RainSandy

ggsave(filename = "RainSandyMap", plot = RainSandy, device = "png", 
       path = "Outputs", width=5, height=6, units="in", dpi = 300, bg = "Black")



# STORM #############

# Setting projection to Lambert Conformal Conic for reports
coordinates(tidy_storm)<-~x+y
tidy_storm <- data.frame(x=coordinates(tidy_storm)[,1], y=coordinates(tidy_storm)[,2], 
                        tidy_storm@data) 

StormSandy <- ggplot(data = AoI) +
  geom_polygon(aes(x=long, y=lat, group=group), fill="grey20", colour="grey35", alpha=1) + 
  geom_point(data = tidy_storm, aes(x=x, y=y), color="#a6cee3", alpha = 0.5, size=0.15) +
  coord_map("lambert", lat0=20, lat=50)+
  theme(legend.position = "none") +
  coord_equal(ratio=1) +
  labs(x="", y="", title="")+
  coord_equal(ratio=1) + 
  annotate(geom = "text", 
           x = getSpPPolygonsLabptSlots(AoI)[,1],
           y = getSpPPolygonsLabptSlots(AoI)[,2],
           label = AoI$STATE_NAME,
           size = 1.2,
           color = "white") + 
  theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(),# get rid of x and y ticks
        axis.text.y = element_blank(),
        axis.text.x = element_blank(), legend.position = "none", 
        panel.background = element_rect(fill = "grey12", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )


StormSandy

ggsave(filename = "StormSandyMap", plot = StormSandy, device = "png", 
       path = "Outputs", width=5, height=6, units="in", dpi = 300, bg = "Black")



# WIND #############

# Setting projection to Lambert Conformal Conic for reports
coordinates(tidy_wind)<-~x+y
tidy_wind <- data.frame(x=coordinates(tidy_wind)[,1], y=coordinates(tidy_wind)[,2], 
                         tidy_wind@data) 

WindSandy <- ggplot(data = AoI) +
  geom_polygon(aes(x=long, y=lat, group=group), fill="grey20", colour="grey35", alpha=1) + 
  geom_point(data = tidy_wind, aes(x=x, y=y), color="#66c2a5", alpha = 0.5, size=0.15) +
  coord_map("lambert", lat0=20, lat=50)+
  theme(legend.position = "none") +
  coord_equal(ratio=1) +
  labs(x="", y="", title="")+
  coord_equal(ratio=1) + 
  annotate(geom = "text", 
           x = getSpPPolygonsLabptSlots(AoI)[,1],
           y = getSpPPolygonsLabptSlots(AoI)[,2],
           label = AoI$STATE_NAME,
           size = 1.2,
           color = "white") + 
  theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(),# get rid of x and y ticks
        axis.text.y = element_blank(),
        axis.text.x = element_blank(), legend.position = "none", 
        panel.background = element_rect(fill = "grey12", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )


WindSandy

ggsave(filename = "WindSandyMap", plot = WindSandy, device = "png", 
       path = "Outputs", width=5, height=6, units="in", dpi = 300, bg = "Black")



