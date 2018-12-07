library(stringr) # For str_to_lower
library(tidyverse) # For ggplot. Nice plots.
library(lubridate) # Play nicely with dates
library(sf) # Spatial monster


# Defining working directory
setwd ("Chapter1/Harvey")

harvey <- readRDS("HarveySH_withLatLon.rds")

harvey$text <- str_to_lower(harvey$text)



####################### DATA READING AND PROJECTING ###########################


# Format date/time
harvey$date <- strptime(harvey$created_at, "%a %b %d %H:%M:%S %z %Y", tz = "UTC") 

harvey$date <- ymd_hms(harvey$date, tz = "UTC") #maybe unnecessary

# Store tweets as simple features and project data
harvey_tweets_sf <- harvey %>% 
  select(lat = lat, 
         lon = lon, 
         date = date, 
         tweet = text) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% # set WGS84 as original datum
  st_transform(crs ="+proj=lcc +lat_1=20 + lat_2=60 + lat_0=40 + 
               lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +
               units=m no_defs") # Projected in North_America_Lambert_Conformal_Conic

# st_crs(harvey_tweets_sf) # Retrieve current coord ref system: EPSG: 4326 WGS84