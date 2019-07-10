library(stringr) # For str_to_lower
library(tidyverse) # For ggplot. Nice plots.
library(lubridate) # Play nicely with dates
library(sf) # Spatial monster
library(leaflet)
library(dbscan)


### THIS IS WITH THE DATA I DOWNLOADED SO IT'S NOT WORKING

# Defining working directory
setwd ("../Chapter1/Harvey")

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



######################### DEFINING TEMPORAL STAGES #####################################

# Creating an attribute to define flood_stage for each report

min_datetime <- min(harvey_tweets_sf$date)
max_datetime <- max(harvey_tweets_sf$date)

# TO BE DEFINED!!!


######################### SPATIAL CLUSTERING ####################################

# set.seed(123)
# 
# clusters <- hdbscan(harvey_tweets_sf %>%
#                       st_coordinates(), #This rounds coordinates
#                     minPts = 350) # NEED TO RUN THIS IN ANOTHER COMPUTER
# 
# colo_clusters <- colo_tweets_sf %>% 
#   mutate(cluster = clusters$cluster)
# 
# ## Plotting spatial cluster results
# 
# colo_clusters <- colo_clusters %>% # Need to reproject in WGS84 datum. long lat format.
#   st_transform(crs = 4326)
# 
# colo_clusters$cluster <- as.factor(colo_clusters$cluster) #Clusters as factors for coloring
# pal <- colorFactor(c("#636363", "red", "Blue"), domain = c("0", "1", "2"))
# 
# # #1. or Red cluster has 608 tweets. Denver
# # #2. or Blue cluster has 1998 tweets. Boulder
# # 2219/4840 tweets were classified as outliers.
# 
# coloMap <- leaflet(colo_clusters) %>% # Interactive map to see resulting clusters
#   addTiles()  %>%
#   addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>% 
#   addCircles(weight = 3, 
#              radius=40,
#              color= ~pal(cluster), 
#              stroke = TRUE, 
#              fillOpacity = 0.7,
#              popup = ~htmlEscape(cluster))%>% 
#   setView(lng = -94, lat = 40.4, zoom = 4.5)
# 
# coloMap



######################### HISTOGRAM ###########################################

ggplot(harvey_tweets_sf, aes(x = harvey_tweets_sf$date)) +
  geom_histogram(aes(y = ..count..), binwidth = 3600, 
                 position="identity", alpha =0.9)
