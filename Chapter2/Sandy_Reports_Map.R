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

setwd ("/home/marcela/Documents/ICC") 



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

#ggplot() +  geom_polygon(data=AoI, aes(x=long, y=lat, group=group))

Sandy <- read.csv("Shelton_et_al_Sandy.csv", header = TRUE, sep = ",")

# class(Sandy) # data.frame
coordinates(Sandy)<-~longitude+latitude 
# class(Sandy) # [1] "SpatialPointsDataFrame"
proj4string(AoI) <- proj4string(Sandy)
inside.area <- !is.na(over(Sandy, as(AoI, "SpatialPolygons")))

sandy.inside<-Sandy[inside.area, ]
class(sandy.inside)
SandyEC<- data.frame(x=coordinates(sandy.inside)[,1], y=coordinates(sandy.inside)[,2],
                     sandy.inside@data) 
write.csv(SandyEC, "Sandy_Subset.csv", row.names=FALSE)

dim(SandyEC)

# Alpha blending (transparency) to make the points transparent. In alpha as a
# ratio, the denominator gives the number of points that must be overplotted
# to give a solid colour.
#make sure that the an equal length on both axis represents the same change in units.
ggplot(AoI)+geom_polygon(aes(x=long, y=lat, group=group), fill="grey20", 
      colour="grey35", alpha=1) +
      labs(x="", y="", title="")+ #labels
  geom_point(data=SandyEC, aes(x=x, y=y, color="cyan"), alpha = 1 / 8, size=0.05) +
        theme(legend.position = "none") +
      coord_equal(ratio=1) +
      annotate(geom = "text", 
           x=getSpPPolygonsLabptSlots(AoI)[,1], 
           y=getSpPPolygonsLabptSlots(AoI)[,2],
           label = AoI$STATE_NAME,
           size = 2,
           color = "white") +
      theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(),# get rid of x and y ticks
            axis.text.y = element_blank(),
            axis.text.x = element_blank(), legend.position = "none", 
            panel.background = element_rect(fill = "grey12", colour = NA),
            panel.grid.major = element_line(colour = "grey18"),
            panel.grid.minor = element_line(colour = "grey18", size = 0.25)
            )






