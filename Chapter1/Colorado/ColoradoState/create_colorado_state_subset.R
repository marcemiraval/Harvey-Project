# This piece of code was before in the script Colorado_Reports_Map and I just put it here to make it more readable.
# Also this was done a while ago so I'm not using tidyverse.

# 1. Creates the subset containing tweets sent only from Colorado State
# 2. Generates a polygon of Colorado State with counties


library(rgdal)

# Defining working directory
setwd ("Chapter1/Colorado/ColoradoState")

US <- readOGR(dsn = "cb_2016_us_county_20m/cb_2016_us_county_20m.shp")

# Setting projection to Lambert Conformal Conic
us.prjd <- spTransform(US, CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 
                                 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")) 

# Filtering Out polygon for Colorado
coloradoPol <- subset(us.prjd, (STATEFP =="08")) # 

coloradoR <- read.csv("Colorado_Clean.csv", header = TRUE, sep = ",")
coordinates(coloradoR)<-~longitude+latitude  #Makes coloradoR a spatial object
proj4string(coloradoR) <- CRS("+init=epsg:4326") #set datum |4326 is the epsg identifier of wgs 84

coloradoR <- spTransform(coloradoR, CRSobj = CRS(proj4string(coloradoPol)))

inside.area <- !is.na(over(coloradoR, as(coloradoPol, "SpatialPolygons")))

colorado.inside<-coloradoR[inside.area, ]
ColoradoR_co<- data.frame(x=coordinates(colorado.inside)[,1], y=coordinates(colorado.inside)[,2],
                          colorado.inside@data) 

write.csv(ColoradoR_co, "Outputs/Colorado_State_Subset.csv", row.names=FALSE)
saveRDS(coloradoPol, file = "Outputs/colorado_state_poly.rds")
