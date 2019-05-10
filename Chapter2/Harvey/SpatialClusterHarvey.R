
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


######################### IMPORTING DATA ###########################################

# Importing Tweets
TweetsHarveyTexas_sf <- readRDS(file = "Data/TweetsHarveyTexas_sf.rds")  ## havent used this yet
TweetsHarveyTexas <- readRDS(file = "Data/TweetsHarveyTexas.rds")

# Importing Spotters
HarveyNWS <- readRDS(file = "Data/HarveyNWSTexas.rds")  ## havent used this yet

# Importing allReports dataset
allReports <- readRDS(file = "Data/allReports.rds")  ## havent used this yet


####################### DATA CLEANING ###############################

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



######################### SPATIAL CLUSTERING ####################################

set.seed(123)

clusters <- hdbscan(TweetsHarveyTexas_sf %>%
                      st_coordinates(), #This rounds coordinates
                    minPts = 55)

harvey_clusters <- TweetsHarveyTexas_sf %>% 
  mutate(cluster = clusters$cluster)

## Plotting spatial cluster results

harvey_clusters <- harvey_clusters %>% # Need to reproject in WGS84 datum. long lat format.
  st_transform(crs = 4326)

harvey_clusters$cluster <- as.factor(harvey_clusters$cluster) #Clusters as factors for coloring
pal <- colorFactor(c("#636363", "#8856a7", "#f1a340", "#c51b7d", "#80cdc1","red", "green"), 
                   domain = c("0", "1", "2", "3", "4", "5", "6"))

# #1. or Red cluster has 608 tweets. Denver
# #2. or Blue cluster has 1998 tweets. Boulder
# 2219/4840 tweets were classified as outliers.

harveyMap <- leaflet() %>% # Interactive map to see resulting clusters
  addTiles()  %>%
  addProviderTiles(providers$OpenStreetMap) %>% 
  addEsriFeatureLayer(
    url = paste0("https://services.arcgis.com/VTyQ9soqVukalItT/arcgis/rest/services/",
                 "DR4332_TX_Observed_Flood_Extent/FeatureServer/0"),
    useServiceSymbology = FALSE,
    stroke = FALSE,
    fillColor = "Blue",
    fillOpacity = 0.3) %>% 
  addCircles(data = harvey_clusters,
             weight = 3, 
             radius=40,
             color= ~pal(cluster), 
             stroke = TRUE, 
             fillOpacity = 1,
             popup = ~htmlEscape(cluster)) %>% 
  setView(lng = -96.5, lat = 30, zoom = 7.1)



harveyMap

