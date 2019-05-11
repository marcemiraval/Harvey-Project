
library(tidyverse) # For ggplot. Nice plots.
library(sf) # Spatial monster

# For spatial clustering
library(dbscan)
# library(fpc) # Check if this is necessary
library(leaflet)
library(leaflet.esri)
library(htmltools)

# Defining working directory
setwd ("Chapter2")


# IMPORTING DATA ----------------------------------------------------------

# Importing Tweets
TweetsHarveyTexas_sf <- readRDS(file = "Data/TweetsHarveyTexas_sf.rds")  ## havent used this yet
TweetsHarveyTexas <- readRDS(file = "Data/TweetsHarveyTexas.rds")

# Importing Spotters
HarveyNWS <- readRDS(file = "Data/HarveyNWSTexas.rds")  

# Store tweets as simple features and project data
HarveyNWS_sf <- HarveyNWS %>% 
  st_as_sf(coords = c("BEGIN_LON", "BEGIN_LAT"), crs = 4326) # Check the thing about begin

# Importing allReports dataset
allReports <- readRDS(file = "Data/allReports.rds")  ## havent used this yet



# DATA CLEANING -----------------------------------------------------------

TweetsHarveyTexas_sf <- TweetsHarveyTexas_sf %>% # Removing retweets
  filter(!str_detect(tweet, "^RT"))

TweetsHarveyTexas_sf$tweet <- str_to_lower(TweetsHarveyTexas_sf$tweet) # Converting text to lower_case letter

###   WHILE THESE TERMS ARE IDENTIFIED
#TweetsToExclude <- c("i'm at", "vegas", "#job", "tweetmyjobs",
#                     "i like that b", "playboy ranks")


# Here is to remove tweets from bots
# grepl function doesn't take all elements in the vector.
# So we have to paste them with an or stament like "i'm at|vegas"

#### FOR NOW
#colorado <- colorado[!grepl(paste(TweetsToExclude, collapse = "|"), colorado$t_text),] 



# FLOOD EXTENT ------------------------------------------------------------

FloodExtent <- st_read("Data/ObservedFloodExtent/ObservedFloodExtent.shp")
FloodExtent <- FloodExtent %>% # Need to reproject in WGS84 datum. long lat format.
  st_transform(crs = 4326)

# Checking coordinate systems
st_crs(FloodExtent)

bb = AOI::getBoundingBox(w) %>% st_as_sf()


# SPATIAL CLUSTERING ------------------------------------------------------


set.seed(123)

clusters <- hdbscan(TweetsHarveyTexas_sf %>%
                      st_coordinates(), #This rounds coordinates
                    minPts = 55)

harvey_clusters <- TweetsHarveyTexas_sf %>% 
  mutate(cluster = clusters$cluster)

# Checking coordinate systems
st_crs(harvey_clusters)

## Plotting spatial cluster results
harvey_clusters$cluster <- as.factor(harvey_clusters$cluster) #Clusters as factors for coloring
pal <- colorFactor(c("#bababa", "RED", "#bf812d", "#c51b7d", "#f46d43", "#3288bd", "#762a83"), 
                   domain = c("0", "1", "2", "3", "4", "5", "6"))

# #0. or Grey - No cluster. 187 tweets.
# #1. or Red - Tweets sent from a single location. Needs to be removed after content analysis. 56 tweets.
# #2. or Brown - Dallas. 171 tweets.
# #3. or Futsia - Beaumont. 83 tweets.
# #4. or Orange - Houston. 3242 tweets.
# #5. or Blue - San Antonio y Austin. 561 tweets.
# #6. or Purple - Corpus Christi. 184 tweets.

# a <- harvey_clusters %>% 
#   filter(cluster == "6")

#1d91c0

harveyMap <- leaflet() %>% # Interactive map to see resulting clusters
  addTiles()  %>%
  addProviderTiles(providers$Stamen.TonerLite) %>% 
  addCircleMarkers(data = HarveyNWS_sf,
                   weight = 5, 
                   radius = 10,
                   stroke = FALSE,
                   color = "#35978f",
                   fill = TRUE,
                 #  fillColor = "#35978f",
                   opacity = 0.8) %>% #With fillOpacity is less transparent
  addCircleMarkers(data = harvey_clusters,
                   weight = 5,
                   radius= 3,
                   color= ~pal(cluster),
                   stroke = FALSE,
                   fill = TRUE,
                   fillOpacity = 0.5,
                   popup = ~htmlEscape(cluster)) %>%
  setView(lng = -96.5, lat = 31.5, zoom = 6.499999999999995)



harveyMap

######## 
# This is what I would need if want to read the FloodExtent feature layer from the ESRI map service
#   addEsriFeatureLayer(
#     url = paste0("https://services.arcgis.com/VTyQ9soqVukalItT/arcgis/rest/services/",
#                  "DR4332_TX_Observed_Flood_Extent/FeatureServer/0"),
#     useServiceSymbology = FALSE,
#     stroke = FALSE,
#     fillColor = "Blue",
#     fillOpacity = 0.3)

clusteredTweets <- harvey_clusters %>% 
  filter(cluster != 0)

intersection <- st_intersection(x = FloodExtent, y = clusteredTweets)


