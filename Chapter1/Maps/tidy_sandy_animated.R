library(maps)
library(ggplot2)
library(gganimate)
library(sp)
library(maptools)
library(dplyr)

setwd("Chapter1/Sandy")

latlong2state <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, states_sp)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}

tidy_sandy <- readRDS(file = "EastCoast/tidy_sandy.rds")

east <- data.frame(x = tidy_sandy$x, y = tidy_sandy$y)
tidy_sandy$state<-latlong2state(east)
east_state<-c("maryland", "delaware", "new jersey","new york","connecticut",
              "massachusetts","rhode island","north carolina","virginia",
              "west virginia","ohio","pennsylvania","new hampshire")
sandy_east=filter(tidy_sandy,state %in% east_state)

ggplot(sandy_east, aes(x=x, y=y, color = "grey",
                                   frame=created_at))+
  borders("state","maryland",fill="#252525")+
  borders("state","delaware",fill="#252525")+
  borders("state","new jersey",fill="#252525")+
  borders("state","new york",fill="#252525")+
  borders("state","connecticut",fill="#252525")+
  borders("state","massachusetts",fill="#252525")+
  borders("state","rhode island",fill="#252525")+
  borders("state","north carolina",fill="#252525")+
  borders("state","virginia",fill="#252525")+
  borders("state","west virginia",fill="#252525")+
  borders("state","ohio",fill="#252525")+
  borders("state","new hampshire",fill="#252525")+
  borders("state","pennsylvania",fill="#252525")+
  geom_point(size=0.2)+
  coord_map("lambert", lat0=20, lat=50)+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_rect(fill="black"),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_rect(fill="black"),
        plot.title = element_text(family="Garamond", hjust=0.5,lineheight=.8, 
                                  colour = "white",size=16)) +
  transition_time(created_at) +
  labs(title = "{round(frame_time, 0)}")

sandyEastAnim <- last_animation()

anim_save("EastCoast/sandyEastAnimation", animation = last_animation())

sandyEastAnim  <- anim_save()

# gganimate(east_coast,ani.width=1200, ani.height=800,interval=0.2,"east coast.gif")  
# This is not working but I will keep it to use those parameters (interval) with the new API










