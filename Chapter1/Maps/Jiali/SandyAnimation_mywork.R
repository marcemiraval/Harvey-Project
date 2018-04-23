library(rgdal)
library(ggplot2)
library(gganimate)
library(maps)
library(sp)
library(maptools)
library(GISTools) # data tornados

data(tornados) 

setwd('/home/marcela/Coding/EWE-reporting/Maps/Jiali')
Sandy=read.csv("Sandy.csv")
Sandy=na.omit(Sandy)
Sandy<-Sandy[!(Sandy$longitude<=-145 | Sandy$latitude>=50 | Sandy$latitude<=28),]
Sandy$date=as.Date(Sandy$created_at)
Sandy$date_time=as.POSIXct(Sandy$created_at, format="%Y-%m-%d %H")
Sandy=Sandy[-c(4)]

coordinates(Sandy)<-~longitude+latitude #Makes Sandy a spatial object
proj4string(Sandy) <- CRS("+init=epsg:4326") #set datum |4326 is the epsg identifier of wgs 84

# Define Projection
SandyProj <- spTransform(Sandy, CRS("+proj=lcc +lat_1=50 +lat_2=30 +lon_0=-100 +ellps=WGS84")) # might change later to +lon_0=-75

# Convert back to data frame to use ggplot
Sandy_df <- data.frame(SandyProj)
names(Sandy_df)[names(Sandy_df)=="longitude"]<-"x"
names(Sandy_df)[names(Sandy_df)=="latitude"]<-"y"

# Filtering states affected by Sandy according to SHELDUS database in order to create AoI
index <- us_states$STATE_NAME == "Maryland" | us_states$STATE_NAME == "Delaware"| 
  us_states$STATE_NAME == "New Jersey" | us_states$STATE_NAME == "New York"|
  us_states$STATE_NAME == "Connecticut" | us_states$STATE_NAME == "Massachusetts"|
  us_states$STATE_NAME == "Rhode Island"| us_states$STATE_NAME == "North Carolina"|
  us_states$STATE_NAME == "Virginia" | us_states$STATE_NAME ==  "West Virginia"|
  us_states$STATE_NAME == "Ohio"| us_states$STATE_NAME ==  "Pennsylvania"|
  us_states$STATE_NAME == "New Hampshire"   

AoI <- us_states[index,]
proj4string(AoI) <- CRS("+init=epsg:4326") #set datum |4326 is the epsg identifier of wgs 84
AoIProj<-spTransform(AoI,CRS("+proj=lcc +lat_1=50 +lat_2=30 +lon_0=-100 +ellps=WGS84"))

proj4string(us_states) <- CRS("+init=epsg:4326") #set datum |4326 is the epsg identifier of wgs 84
us_states_proj<-spTransform(us_states,CRS("+proj=lcc +lat_1=50 +lat_2=30 +lon_0=-100 +ellps=WGS84"))

base<-ggplot(Sandy_df,aes(x,y,frame=Sandy_df$date))+
  borders("state",colour=NA,fill="black")+
  geom_point(size=0.2,color="red") +
  coord_equal()
gganimate(base,ani.width=1200, ani.height=800,interval=0.2,"output.gif")  
base

ggplot(states)




