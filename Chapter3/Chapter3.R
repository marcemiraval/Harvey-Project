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
  


