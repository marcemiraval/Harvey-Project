library(maps)
library(ggplot2)
library(gganimate)
library(sp)
library(maptools)
library(dplyr)

setwd('/Users/jialiluan/Downloads')
Sandy=read.csv("Sandy.csv")
Sandy=na.omit(Sandy)
Sandy<-Sandy[!(Sandy$longitude<=-145 | Sandy$latitude>=50 | Sandy$latitude<=28),]
Sandy$date=as.Date(Sandy$created_at)
Sandy$date_time=as.POSIXct(Sandy$created_at, format="%Y-%m-%d %H")
Sandy=Sandy[-c(4)]

usa <-ggplot(Sandy, aes(x=longitude, y=latitude, color = "firebrick4",frame=date_time))+
  borders("state",fill="#252525",size=0.1)+
  geom_point(size=0.2)+
  coord_map("lambert", lat0=20, lat=50)+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_rect(fill="black"),
        plot.title = element_text(hjust=0.5,lineheight=.8, face="bold",colour = "white",size=24),
        plot.subtitle = element_text(colour="white",hjust = 0.5,size=12),
        plot.caption = element_text(colour = "white",size=12))+
  labs(title="Twitter Report on Hurricane Sandy \n",
       subtitle = "Animated Map",
       caption ="Projection: Lambert \n Data Source: Shelton \n Jiali Luan \n June 10th, 2017")
     
gganimate(usa,ani.width=1200, ani.height=800,interval=0.2,"usa.gif")  

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

east <- data.frame(x=Sandy$longitude,y=Sandy$latitude)
Sandy$state<-latlong2state(east)
east_state<-c("maryland", "delaware", "new jersey","new york","connecticut",
           "massachusetts","rhode island","north carolina","virginia","west virginia","ohio",
           "pennsylvania","new hampshire")
Sandy_east=filter(Sandy,state %in% east_state)

east_coast<-ggplot(Sandy_east, aes(x=longitude, y=latitude, color ="firebrick4",frame=date))+
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
       panel.background=element_blank(),
       panel.border=element_blank(),
       panel.grid.major=element_blank(),
       panel.grid.minor=element_blank(),
       plot.background=element_rect(fill="black"),
       plot.title = element_text(hjust=0.5,lineheight=.8, face="bold",colour = "white",size=24),
       plot.subtitle = element_text(colour="white",hjust = 0.5,size=12),
       plot.caption = element_text(colour = "white",size=12))+
  labs(title="Twitter Report on Hurricane Sandy in the East Coast \n",
       caption ="Data Source: Dolly Project")
gganimate(east_coast,ani.width=1200, ani.height=800,interval=0.2,"east coast.gif")  




