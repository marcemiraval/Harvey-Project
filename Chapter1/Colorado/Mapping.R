library(sf)
library(tmap)
library(tidyverse) 
library(dbscan)

setwd("Chapter1/Colorado")

floodExtent <- st_read( "GroundTruthData/Flood2013Extents/Flood2013Extents.shp")
arvadaFlood <- st_read("GroundTruthData/Arvada_Flood_Zones/FEMA_Floodplain.shp")
cityFlood <- st_read("GroundTruthData/CityFloodplain/CityFloodplain.shp")
creekFlood <- st_read("GroundTruthData/Flood2013Comprehensive_Creek_Planning_Initiative_Areas/Flood_2013_Comprehensive_Creek_Planning_Initiative_Areas.shp")
inundated <- st_read("GroundTruthData/Flood_2013_Inundated_Areas/Flood_2013_Inundated_Areas.shp")
prelim_city <- st_read("GroundTruthData/Partial_Preliminary_2013_Colorado_Flooding_Extents/CityofBoulder.shp")
prelim_evans <- st_read("GroundTruthData/Partial_Preliminary_2013_Colorado_Flooding_Extents/Evans_SouthPlatte_Evans.shp")
prelim_left_Longmont <- st_read("GroundTruthData/Partial_Preliminary_2013_Colorado_Flooding_Extents/LeftHand_MountainstoLongmont.shp")
prelim_JamesCreek <- st_read("GroundTruthData/Partial_Preliminary_2013_Colorado_Flooding_Extents/LittleJamesCreek_JamestowntoLeftHandCreek.shp")
prelim_SouthPlatte <- st_read("GroundTruthData/Partial_Preliminary_2013_Colorado_Flooding_Extents/SouthPlatte_Sterling_SterlingtoOvid.shp")
prelim_cityLongmont <- st_read("GroundTruthData/Partial_Preliminary_2013_Colorado_Flooding_Extents/StVrain_Longmont_CityofLongmont.shp")
prelim_lyonstolong <- st_read("GroundTruthData/Partial_Preliminary_2013_Colorado_Flooding_Extents/StVrain_Lyons_Colorado80toLongmont.shp")
prelim_townsuperior <- st_read("GroundTruthData/Partial_Preliminary_2013_Colorado_Flooding_Extents/TownofSuperior.shp")


floodExtent <- floodExtent %>% 
  st_transform(4326)
arvadaFlood <- arvadaFlood %>% 
  st_transform(4326)
cityFlood <- cityFlood %>% 
  st_transform(4326)
inundated <- inundated %>% 
  st_transform(4326)
prelim_city <- prelim_city %>% 
  st_transform(4326)
prelim_evans <- prelim_evans %>% 
  st_transform(4326)
prelim_left_Longmont <- prelim_left_Longmont %>% 
  st_transform(4326)
prelim_JamesCreek <- prelim_JamesCreek %>% 
  st_transform(4326)
prelim_SouthPlatte <- prelim_SouthPlatte %>% 
  st_transform(4326)
prelim_cityLongmont <- prelim_cityLongmont %>% 
  st_transform(4326)
prelim_lyonstolong <-  prelim_lyonstolong %>% 
  st_transform(4326)
prelim_townsuperior <- prelim_townsuperior %>% 
  st_transform(4326)

damagePols <- st_read("GroundTruthData/StructuresPols/DamagePolsAllIncluded.geojson")

tweetsResponse <- read_csv("colo_Flood_IA.csv")
tweets <- tweetsResponse %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)


minPts <- function(a) {
  cuantox <- a*nrow(tweets)/100
  return(cuantox)
}

lista <- seq(1, 10, by = 1) 

minPts_list <- lapply(lista, minPts)

set.seed(123)

clusters <- hdbscan(tweets %>%
                      st_coordinates(), #This rounds coordinates
                    minPts = 160)

colo_clusters <- tweets %>% 
  mutate(cluster = clusters$cluster)

## Plotting spatial cluster results

colo_clusters <- colo_clusters %>% # Need to reproject in WGS84 datum. long lat format.
  st_transform(crs = 4326)

colo_clusters$cluster <- as.factor(colo_clusters$cluster) #Clusters as factors for coloring

tmap_mode("view") 

map <- tm_shape(damagePols) + 
  tm_polygons(col = "grey",
              alpha = 0.8) +
  tm_shape(tweets) + 
  tm_dots(col = "red",
          scale=1,
          alpha = 0.5) +
  tm_shape(colo_clusters) + 
  tm_dots(col = "cluster",
          alpha = 1)

map + tm_basemap(server = "Stamen.TonerLite")

map1 <- tm_shape(floodExtent) + 
  tm_polygons(col = "#756bb1",
              alpha = 0.8) +
  tm_shape(arvadaFlood) + 
  tm_polygons(col = "#756bb1",
              alpha = 0.8) +
  tm_shape(cityFlood) + 
  tm_polygons(col = "#756bb1",
              alpha = 0.8) +
  tm_shape(creekFlood) + 
  tm_polygons(col = "#756bb1",
              alpha = 0.8) +
  tm_shape(inundated) + 
  tm_polygons(col = "#756bb1",
              alpha = 0.8) +
  tm_shape(prelim_city) + 
  tm_polygons(col = "#756bb1",
              alpha = 0.8) +  
  tm_shape(prelim_evans) + 
  tm_polygons(col = "#756bb1",
              alpha = 0.8) +
  tm_shape(prelim_left_Longmont) + 
  tm_polygons(col = "#756bb1",
              alpha = 0.8) +
  tm_shape(prelim_JamesCreek) + 
  tm_polygons(col = "#756bb1",
              alpha = 0.8) +
  tm_shape(prelim_SouthPlatte) + 
  tm_polygons(col = "#756bb1",
              alpha = 0.8) +
  tm_shape(prelim_cityLongmont) + 
  tm_polygons(col = "#756bb1",
              alpha = 0.8) +
  tm_shape(prelim_lyonstolong) + 
  tm_polygons(col = "#756bb1",
              alpha = 0.8) +
  tm_shape(prelim_townsuperior) + 
  tm_polygons(col = "#756bb1",
              alpha = 0.8) +
  tm_shape(damagePols) + 
  tm_polygons(col = "grey",
              alpha = 0.8) +
  tm_shape(tweets) + 
  tm_dots(col = "red",
          scale=1,
          alpha = 0.5) +
  tm_shape(colo_clusters) + 
  tm_dots(col = "cluster",
              alpha = 1)

map1 + tm_basemap(server = "Stamen.TonerLite")


