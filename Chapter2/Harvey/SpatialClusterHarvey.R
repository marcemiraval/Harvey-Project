
# LIBRARIES ---------------------------------------------------------------

library(tidyverse) # For ggplot. Nice plots.
library(tibble)
library(sf) # Spatial monster
library(sp)

# For spatial clustering
library(dbscan)

# Mapping tools
library(leaflet)
library(leaflet.esri)
library(htmltools)
library(raster)
library(rmapshaper)
library(tigris) # Loading tigris dataset/ Census
library(htmlwidgets)
library(classInt)
library(RColorBrewer)
library(mapview)
library(classInt)
library(tmap)

# For text Mining
library(tm) 
library(wordcloud)
library(tidytext)
library(stringr) # For str_to_lower
library(stringi)

options(tigris_use_cache = TRUE)

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

# TweetsHarveyTexas_sf$tweet <-  iconv(TweetsHarveyTexas_sf$tweet,
#                                      "UTF-8",
#                                      "UTF-8",
#                                      sub='')
# 
# TweetsHarveyTexas_sf$tweet <- str_to_lower(TweetsHarveyTexas_sf$tweet) # Converting text to lower_case letter
# 
# TweetsToExclude <- c("stay safe", "prayers", "be safe", "donations")
# 
# 
# # Here is to remove tweets from bots
# # grepl function doesn't take all elements in the vector.
# # So we have to paste them with an or stament like "i'm at|vegas"
# TweetsHarveyTexas_sf <- TweetsHarveyTexas_sf[!grepl(paste(TweetsToExclude, collapse = "|"), 
#                                                     TweetsHarveyTexas_sf$tweet),] 



# FLOOD EXTENT ------------------------------------------------------------
# 
# FloodExtent <- st_read("Data/ObservedFloodExtent/ObservedFloodExtent.shp") %>% # Need to reproject in WGS84 datum. long lat format.
#   st_transform(crs = 4326)
# 
# 
# # In flood we save the "original" or the first simplified version resulting in QGIS. Unable to use the original file for this.
# # But for the real version I just need to run simplifying steps used below with the original file 
# # downloaded from: http://www.arcgis.com/home/item.html?id=826e2679f912443d9c7853d3addd59aa
# # I can't do that now because it will take so much time I don't have.
# 
# flood <- st_read("Data/FloodSimplified/femaFloodProjSimplyCheck.shp")
# 
# # Here to simplify
# flood_simply <- ms_simplify(input = flood,
#                             keep = 0.025)
# 
# # Saving/exporting result
# saveRDS(flood_simply, file = "Data/flood_simply.rds")
# st_write(flood_simply, "Data/flood_simply.shp")

# Load simplified file
flood_simply <- readRDS(file = "Data/flood_simply.rds")

flood_simply <- flood_simply %>% # Need to reproject in WGS84 datum. long lat format.
  st_transform(crs = 4326)

# Checking coordinate systems
st_crs(flood_simply)

# Fixing Topology error
checker <- st_is_valid(flood_simply)
good <- flood_simply[!is.na(checker),]
good <- good[checker,] # "good" is the final version of the final flood file in vector format.

# Creating raster Mike's way
# bb = AOI::getBoundingBox(fvu) %>% st_as_sf()
# r = raster(bb)
# tmp = fasterize::fasterize(fv, r)

# Make a raster of the flood areas. This is great for plotting
# rraster <- raster()
# extent(rraster) <- extent(good)
# dim(rraster) = c(5000, 5000)
# # res(rraster) <- 5 # set cell size to 2500 metres Also I can play with this later.
# floodR <- fasterize::fasterize(good, rraster)
# saveRDS(floodR, file = "Data/floodR.rds")

floodR <- readRDS(file = "Data/floodR.rds")

# FLOOD FILTER ------------------------------------------------------

TexasCounties <- readRDS(file = "Data/TexasCounties.rds") # Total area in meters: 128295306849

texas <- TexasCounties  %>% 
  st_transform(crs = 4326)

TweetsInCounties <- st_intersection(x = texas, y = TweetsHarveyTexas_sf) # Tweets in counties
saveRDS(TweetsInCounties, file = "Data/TweetsInCounties.rds") # 3808 tweets

outside <- sapply(st_intersects(TweetsHarveyTexas_sf, texas),function(x){length(x)==0}) 
TweetsOut <- TweetsHarveyTexas_sf[outside, ] # Explanation here: https://gis.stackexchange.com/questions/245136/how-to-subset-point-data-by-outside-of-polygon-data-in-r  
saveRDS(TweetsOut, file = "Data/TweetsOutsideCounties.rds") # 676 tweets outside counties

TweetsInFlood <- st_intersection(x = good, y = TweetsHarveyTexas_sf) # Tweets in counties
saveRDS(TweetsInFlood, file = "Data/TweetsInFlood.rds") #83 tweets


# ANALYSIS IN HEXAGONS -------------------------------------------------------------------

# # When using the whole state area
# 
# # Data to create basemap
# states <- states(cb = FALSE, resolution = "500k") # needs tigris package
# state_sf <- st_as_sf(states)
# texas <- state_sf %>% 
#   filter(NAME == "Texas") %>% 
#   st_transform(crs = 4326)
# 
# # Checking CRS
# st_crs(texas)
# 
# texas_sp <-  as(texas, "Spatial")

texas_sp <-  as(texas, "Spatial")


## Defining number of hexagons works best to cover the whole area
sp_hex <- HexPoints2SpatialPolygons(spsample(texas_sp,
                                             n=12500, #Try with 25000 first and it was probably too small. With 12500 polygons, hex area is around 10km2
                                             type="hexagonal")) # Create hexagons based on defined number of hex

sf_hex <- st_as_sf(sp_hex) %>% 
  mutate(group = 1:nrow(.))

# HexagonsWithTweets
hexWithTweets <- st_join(sf_hex, TweetsInCounties) %>% 
  group_by(group) %>%
  summarise(total = sum(!is.na(group)))%>% 
  mutate(totRatio = total/sum(total))# Add column that normalize totals

# HexagonsWithSpotters
hexWithSpotters <- st_join(sf_hex, HarveyNWS_sf) %>% 
  group_by(group) %>%
  summarise(total = sum(!is.na(group))) %>% 
  mutate(totRatio = total/sum(total))# Add column that normalize totals

classes <- 6
style_method <- "fisher"
pal1 <- brewer.pal(classes, "YlOrRd")
palData <- classIntervals(hexWithSpotters$totRatio, n = classes, style=style_method, pal = pal1)
hexWithSpotters$colores <- findColours(palData, pal1)%>%
  as.factor(.)
pal2 <- colorBin(pal1, domain = palData$brks, bins = palData$brks, pretty = FALSE)

SandyHexSpotMap <- leaflet(hexWithSpotters) %>%
  setView(lng = -96.5, lat = 31.5, zoom = 7) %>%
  addProviderTiles(providers$Stamen.TonerLite) %>% 
  addPolygons(fillColor = ~colores,
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "2",
              fillOpacity = 0.7,
              popup = ~htmlEscape(sprintf("Reports per hexagon: %i",
                                          total))) %>%
  addLegend(pal = pal2,
            values = ~total, 
            opacity = 0.7, 
            title = "# of NWS reports",
            position = "bottomright")

SandyHexSpotMap
htmlwidgets::saveWidget(SandyHexSpotMap, file = "SandyHexSpotMap10km.html")


classes <- 7
style_method <- "fisher"
pal1 <- brewer.pal(classes, "YlOrRd")
palData <- classIntervals(hexWithTweets$totRatio, n = classes, style=style_method, pal = pal1)
hexWithTweets$colores <- findColours(palData, pal1)%>%
  as.factor(.)
pal2 <- colorBin(pal1, domain = palData$brks, bins = palData$brks, pretty = FALSE)

SandyHexTweetMap <- leaflet(hexWithTweets) %>%
  setView(lng = -96.5, lat = 31.5, zoom = 7) %>%
  addProviderTiles(providers$Stamen.TonerLite) %>% 
  addPolygons(fillColor = ~colores,
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "2",
              fillOpacity = 0.7,
              popup = ~htmlEscape(sprintf("Reports per hexagon: %f",
                                          total))) %>%
  addLegend(pal = pal2,
            values = ~totRatio, 
            opacity = 0.7, 
            title = "Tweets Density",
            position = "bottomright")

SandyHexTweetMap
htmlwidgets::saveWidget(SandyHexTweetMap, file = "SandyHexTweetMap10km.html")

mix <- leafsync::sync(SandyHexSpotMap, SandyHexTweetMap)
save_html(mix, "mix.html", background = "white", libdir = "lib")



## LEVEL 2 TWEETS!!! ------------------------------------------------------

# FloodTweetsInHex <- 
#   st_join(TweetsInFlood, hexWithSpotters) %>% 
#   filter(total == 2) # 3 tweets sent within flooded areas and sharing hex with Spotters. Content seems to be irrelevant.

# InHexInCounty <- 
#   st_join(TweetsInCounties, hexWithSpotters) 
# 
# saveRDS(InHexInCounty, file = "Data/InHexInCounty.rds") 

InHexInCounty <- readRDS(file = "Data/InHexInCounty.rds")

CountyTweetsInHex <- InHexInCounty %>% 
  filter(total != 1) # 30 tweets sent within counties affected and sharing hex with Spotters. There is promosing content. 

CountyTweetsOutHex <- InHexInCounty %>% 
  filter(total == 1) # 3775 tweets sent within counties affected and but not sharing hex with Spotters. They now need to be classified by content. 



# SPATIAL CLUSTERING ------------------------------------------------------

set.seed(123)

clusters <- hdbscan(TweetsHarveyTexas_sf %>%
                      st_coordinates(), #This rounds coordinates
                    minPts = 60)

harvey_clusters <- TweetsHarveyTexas_sf %>% 
  mutate(cluster = clusters$cluster)

# Checking coordinate systems
st_crs(harvey_clusters)

clusteredTweets <- harvey_clusters %>% 
  filter(cluster != 0)

# intersection <- st_intersection(x = FloodExtent, y = clusteredTweets)
# saveRDS(intersection, file = "Data/TweetsInFlood.rds")

TweetsInFlood <- readRDS(file = "Data/TweetsInFlood.rds")

# st_crs(FloodExtent)
# st_crs(clusteredTweets)

# PLOT: TWEETS SPATIAL CLUSTERS, NWS REPORTS AND FLOODED AREAS ------------

harvey_clusters$cluster <- as.factor(harvey_clusters$cluster) #Clusters as factors for coloring
pal <- colorFactor(c("#bababa", "#762a83", "#bf812d", "#f46d43", "#c51b7d", "#7fbc41"), 
                   domain = c("0", "1", "2", "3", "4", "5"))

# #0. or Grey - No cluster. 187 tweets.
# #1. or Red - Tweets sent from a single location. Needs to be removed after content analysis. 56 tweets.
# #2. or Brown - Dallas. 171 tweets.
# #3. or Futsia - Beaumont. 83 tweets.
# #4. or Orange - Houston. 3242 tweets.
# #5. or Blue - San Antonio y Austin. 561 tweets.
# #6. or Purple - Corpus Christi. 184 tweets.


harveyMap <- leaflet() %>% # Interactive map to see resulting clusters
  addTiles()  %>%
  addProviderTiles(providers$Stamen.TonerLite) %>% 
  addRasterImage(floodR,
                 col = '#4393c3', #
                 opacity = 1) %>%  #0.5
  addCircleMarkers(data = HarveyNWS_sf,
                   weight = 5, 
                   radius = 10,
                   stroke = FALSE,
                   color = "#35978f",
                   fill = TRUE,
                 #  fillColor = "#35978f",
                   fillOpacity = 1) %>% #With fillOpacity is less transparent #0.8
  addCircleMarkers(data = harvey_clusters,
                   weight = 5,
                   radius= 3,
                   color= ~pal(cluster),
                   stroke = FALSE,
                   fill = TRUE,
                   fillOpacity = 1, #0.5
                   popup = ~htmlEscape(cluster)) %>% 
  setView(lng = -96.5, lat = 31.5, zoom = 7.499999999999995)

harveyMap

saveWidget(harveyMap, file = "harveyMapNoOpacity.html")

# This is what I would need if want to read the FloodExtent feature layer from the ESRI map service
#   addEsriFeatureLayer(
#     url = paste0("https://services.arcgis.com/VTyQ9soqVukalItT/arcgis/rest/services/",
#                  "DR4332_TX_Observed_Flood_Extent/FeatureServer/0"),
#     useServiceSymbology = FALSE,
#     stroke = FALSE,
#     fillColor = "Blue",
#     fillOpacity = 0.3)


# CLUSTERSINCLUSTERS ------------------------------------------------------

set.seed(123)

clustersclusters <- hdbscan(TweetsInFlood %>%
                      st_coordinates(), #This rounds coordinates
                    minPts = 60)

harvey_clusters_clusters <- TweetsInFlood %>% 
  mutate(clustercluster = clustersclusters$cluster)

clusteredTweets2 <- harvey_clusters_clusters %>% 
  filter(cluster != 0)

harvey_clusters_clusters$cluster <- as.factor(harvey_clusters_clusters$cluster) #Clusters as factors for coloring
pal <- colorFactor(c("#bababa", "#762a83", "#bf812d", "#f46d43", "#c51b7d", "#7fbc41"), 
                   domain = c("0", "1", "2", "3", "4", "5"))

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
  addCircleMarkers(data = harvey_clusters_clusters,
                   weight = 5,
                   radius= 3,
                   color= ~pal(cluster),
                   stroke = FALSE,
                   fill = TRUE,
                   fillOpacity = 0.5,
                   popup = ~htmlEscape(cluster)) %>% 
  setView(lng = -96.5, lat = 31.5, zoom = 7.499999999999995)

harveyMap

saveWidget(harveyMap, file = "harveyMap.html")



# WORDCLOUDS --------------------------------------------------------------

TweetsHarveyWords <- TweetsHarveyTexas_sf %>%
  st_set_geometry(NULL) %>% 
  dplyr::select(tweet) %>% 
  rename(text = `tweet`) 

ToExclude <- c("texas", "houston", "hurricane", "harvey", "austin")

Harvey_tokens <- TweetsHarveyWords %>%
  unnest_tokens(word, text, "tweets") %>% ## It seems better to use the specific argument to unnest tokens in tweets
  filter(!str_detect(word, "[0-9]"), !str_detect(word, "#"), !word %in% ToExclude)%>% 
  anti_join(stop_words)%>%
  # mutate(word = wordStem(word))%>%
  count(word, sort = TRUE) 

# define color palette
pal <- brewer.pal(8,"Dark2")

# plot the 30 most common words
Harvey_tokens %>% 
  with(wordcloud(word, n, scale=c(4,0.5),
                 min.freq = 7, max.words = 30,
                 random.order = FALSE, 
                 rot.per = FALSE, 
                 use.r.layout = FALSE,
                 color = brewer.pal(5, "Blues"),
                 family = "Helvetica"))

# After this I'll remove stay safe 
