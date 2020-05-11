library(sf)
library(geosphere)
library(tidyverse)
library(tmap)

setwd(dir= "Chapter1/Colorado/")

#### Example

Pts <- read_csv("GroundTruthData/StructuresPols/Example/ExamDamage.csv") # Loading data into R
Pol2 <- st_read("GroundTruthData/StructuresPols/Example/pol2.geojson") # Loading data into R

Pts.df <- Pts%>% # As sf object
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  select("tweet_id")

# Plotting the damage polygons and the example of reports
tmap_mode("view") 
map <- tm_shape(Pol2) + 
  tm_polygons(col = "#756bb1",
              alpha = 0.8) +
  tm_shape(Pts.df) + 
  tm_bubbles(col = "red",
             scale=.5)
map + tm_basemap(server = "Stamen.TonerLite")

# Check if there are points within the polygon. St_within also works for this
pnts <- Pts.df %>% 
  mutate(intersection = as.integer(st_intersects(geometry, Pol2))) 

# Compute distances from points to the closest polygon
dist <- geosphere::dist2Line(p = st_coordinates(pnts), line = st_coordinates(Pol2)[,1:2])

# bind results with original points
pts.wit.dist <- cbind(pnts, dist) %>% 
  mutate(distancee = ifelse(is.na(intersection), distance, 0)) %>% 
  select(-c(intersection, distance))


pts.wit.dist %>% 
  ggplot(aes(distancee, tweet_id)) + # , color = topic
  geom_point(size = 2, alpha = 0.7) +
  labs(x = "Distance",
       y = "Tweet ID",
       title = "Comparing exclusivity and semantic coherence",
       subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity")

###################################################################################################

damagePols <- st_read("GroundTruthData/StructuresPols/DamagePolsAllIncluded.geojson")
tweetAndTopics <- st_read("tweet_and_topic.geojson")

mapview(damagePols) + 
  mapview(tweetAndTopics)

# dist.mat <- geosphere::dist2Line(p = pts, line = wrld_subset)
dist <- geosphere::dist2Line(p = st_coordinates(tweetAndTopics), line = st_coordinates(damagePols)[,1:2])
