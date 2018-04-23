# Loading required libraries
# install.packages("rgeos", dependencies = TRUE)
# install.packages("GISTools", dependencies = TRUE)
# install.packages("maps", dependencies = TRUE)
# install.packages("maptools", dependencies = TRUE)
# install.packages(c("rgdal", "sp"))
# install.packages("ggmap")
# install.packages("ggthemes")
# install.packages("fiftystater")
library(rgdal)
library(sp)
library(GISTools)
library(maps)
library(maptools)
library(ggplot2)
library(ggmap)
library(lattice)
library(ggthemes)
library(fiftystater)

# Data to create basemap
setwd ("/home/marcela/Documents/ICC") 
data(tornados) 
Sandy <- read.csv("Shelton_et_al_Sandy.csv", header = TRUE, sep = ",")

# class(Sandy) # data.frame
coordinates(Sandy)<-~longitude+latitude 
# class(Sandy) # [1] "SpatialPointsDataFrame"
proj4string(us_states) <- proj4string(Sandy)

library(mapdata)
layout(rbind(c(0,2,0,0,0,2),
             c(1,0,1,3,3,0),
             c(1,2,1,3,3,2)),
             heights=c(.8, 0, .3),
             widths=c(0, 1, 0, 0, 1, 2))
par(mar=rep(0, 4))
par(oma=c(8,rep(0, 3)))
layout.show(3)
map("world2Hires", "USA:Alaska")
par(mar=rep(0, 4))
map("state")
par(mar=rep(0, 4))
map("world2Hires", "Hawaii")


class(Sandy)

Sandy.df<- data.frame(x=coordinates(Sandy)[,1], y=coordinates(Sandy)[,2],
                     Sandy@data) 

# Alpha blending (transparency) to make the points transparent. In alpha as a
# ratio, the denominator gives the number of points that must be overplotted
# to give a solid colour.
#make sure that the an equal length on both axis represents the same change in units.
ggplot(us_states)+geom_polygon(aes(x=long, y=lat, group=group), fill="grey20", 
      colour="grey35", alpha=1) +
      labs(x="", y="", title="")+ #labels
  geom_point(data=Sandy.df, aes(x=x, y=y, color="cyan"), alpha = 1 / 8, size=0.05) +
        theme(legend.position = "none") +
      coord_equal(ratio=1) +
      annotate(geom = "text", 
           x=getSpPPolygonsLabptSlots(us_states)[,1], 
           y=getSpPPolygonsLabptSlots(us_states)[,2],
           label = us_states$STATE_NAME,
           size = 2,
           color = "white") +
      theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(),# get rid of x and y ticks
            axis.text.y = element_blank(),
            axis.text.x = element_blank(), legend.position = "none", 
            panel.background = element_rect(fill = "grey12", colour = NA),
            panel.grid.major = element_line(colour = "grey18"),
            panel.grid.minor = element_line(colour = "grey18", size = 0.25)
            )






