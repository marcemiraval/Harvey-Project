# devtools::install_github("mikejohnson51/AOI")

# Load libraries
library(help = AOI)
library(AOI)
library(sf)
library(tidyverse)

# Loading data into R
coloradoClean <- read.csv("Chapter1/Colorado/Colorado_Clean.csv", header = TRUE, sep = ",")

# Saving into RData
save(coloradoClean, file="Chapter3/Data/coloradoClean.RData")

load(file= "Chapter3/Data/coloradoClean.RData")

colorado_sf <- st_as_sf(coloradoClean, 
                        coords = c("longitude", "latitude"), 
                        crs = 4326) # Coords = ("x","y"). Also set WGS84 as datum. 

coloradoBB <- getBoundingBox(colorado_sf)

ggplot() +
  geom_polygon(data = coloradoBB, aes(x=long, y=lat)) +
  geom_point(data = coloradoClean, aes(x = longitude, y = latitude))

counties <- AOI::counties

CO_counties <- st_as_sf(counties) %>% 
  filter(state_name == "Colorado") %>% 
  st_transform(4326)

CO_tweets <- st_join(colorado_sf, CO_counties) %>% 
  filter(state_name == "Colorado")

coloradoBB <- getBoundingBox(CO_tweets)

ggplot() +
  geom_polygon(data = coloradoBB, aes(x=long, y=lat)) +
  geom_point(data = coloradoClean, aes(x = longitude, y = latitude))








