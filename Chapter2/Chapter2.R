library(tidyverse)
library(sf)
library(lwgeom) #compute area polygon
library(leaflet) #colorBin function
library(htmltools)
library(maptools)
library(markdown)
library(grid)
library(gridExtra)
library(mapview) #sync function
library(classInt)
library(RColorBrewer)


load(file= "Chapter2/Data/sandy_boundary_sf.RData")
load(file = "Chapter2/Data/sandySpotIn_sf.RData")
load(file = "Chapter2/Data/sandyTweetsIn_sf.RData")

#Prepare dataset to be used in function
sandySpotIn_sf <- sandySpotIn_sf %>%
  rename_at("EVENT_ID",~"id")  


sandy_boundary_area <- st_area(sandy_boundary_sf)

sandy_boundary_sp <-  as(sandy_boundary_sf, "Spatial") # This doesn't go in data preparation because I guess I will use the sf version of the file later.

nHex <- c(h1000 = 1000, h1500 = 1500, h2000 = 2000, 
          h2500 = 2500, h3000 = 3000, h3500 = 3500, h4000 = 4000, h4500 = 4500,
          h5000 = 5000, h5500 = 5500, h6000 = 6000, h6500 = 6500, h7000 = 7000, 
          h7500 = 7500, h8000 = 8000, h8500 = 8500, h9000 = 9000, h9500 = 9500, 
          h10000 = 10000, h10500 = 10500, h11000 = 11000, h11500 = 11500, 
          h12000 = 12000, h12500 = 12500, h13000 = 13000, h13500 = 13500, 
          h14000 = 14000, h14500 = 14500, h15000 = 15000, h15500 = 15500)

creaHex <- function(hexNum) {
  sp_hex <- HexPoints2SpatialPolygons(spsample(sandy_boundary_sp, n = hexNum, type = "hexagonal"))
  sf_hex <- st_as_sf(sp_hex) %>%
    mutate(group = 1:nrow(.))
}

hexagons <- lapply(nHex, creaHex)# Creates a Large list (5 elements, 5.5 Mb)

computeTotals <- function(hex_dfs, reports) {
  hexWithReports <- st_join(hex_dfs, reports) %>%
    group_by(group) %>%
    summarise(total = sum(!is.na(id))) %>%
    mutate(totRatio = total/sum(total))# Add column that normalize totals
}

TotalsSpotters <- lapply(hexagons, computeTotals, reports = sandySpotIn_sf)
TotalsTweets <- lapply(hexagons, computeTotals, reports = sandyTweetsIn_sf)

sd1000 <- sd(TotalsTweets$h1000$totRatio)/mean(TotalsTweets$h1000$totRatio)*100
sd1500 <- sd(TotalsTweets$h1500$totRatio)/mean(TotalsTweets$h1500$totRatio)*100
sd2000 <- sd(TotalsTweets$h2000$totRatio)/mean(TotalsTweets$h2000$totRatio)*100
sd2500 <- sd(TotalsTweets$h2500$totRatio)/mean(TotalsTweets$h2500$totRatio)*100
sd3000 <- sd(TotalsTweets$h3000$totRatio)/mean(TotalsTweets$h3000$totRatio)*100
sd3500 <- sd(TotalsTweets$h3500$totRatio)/mean(TotalsTweets$h3500$totRatio)*100
sd4000 <- sd(TotalsTweets$h4000$totRatio)/mean(TotalsTweets$h4000$totRatio)*100
sd4500 <- sd(TotalsTweets$h4500$totRatio)/mean(TotalsTweets$h4500$totRatio)*100
sd5000 <- sd(TotalsTweets$h5000$totRatio)/mean(TotalsTweets$h5000$totRatio)*100
sd5500 <- sd(TotalsTweets$h5500$totRatio)/mean(TotalsTweets$h5500$totRatio)*100
sd6000 <- sd(TotalsTweets$h6000$totRatio)/mean(TotalsTweets$h6000$totRatio)*100
sd6500 <- sd(TotalsTweets$h6500$totRatio)/mean(TotalsTweets$h6500$totRatio)*100
sd7000 <- sd(TotalsTweets$h7000$totRatio)/mean(TotalsTweets$h7000$totRatio)*100
sd7500 <- sd(TotalsTweets$h7500$totRatio)/mean(TotalsTweets$h7500$totRatio)*100
sd8000 <- sd(TotalsTweets$h8000$totRatio)/mean(TotalsTweets$h8000$totRatio)*100
sd8500 <- sd(TotalsTweets$h8500$totRatio)/mean(TotalsTweets$h8500$totRatio)*100
sd9000 <- sd(TotalsTweets$h9000$totRatio)/mean(TotalsTweets$h9000$totRatio)*100
sd9500 <- sd(TotalsTweets$h9500$totRatio)/mean(TotalsTweets$h9500$totRatio)*100
sd10000 <- sd(TotalsTweets$h10000$totRatio)/mean(TotalsTweets$h10000$totRatio)*100
sd10500 <- sd(TotalsTweets$h10500$totRatio)/mean(TotalsTweets$h10500$totRatio)*100
sd11000 <- sd(TotalsTweets$h11000$totRatio)/mean(TotalsTweets$h11000$totRatio)*100
sd11500 <- sd(TotalsTweets$h11500$totRatio)/mean(TotalsTweets$h11500$totRatio)*100
sd12000 <- sd(TotalsTweets$h12000$totRatio)/mean(TotalsTweets$h12000$totRatio)*100
sd12500 <- sd(TotalsTweets$h12500$totRatio)/mean(TotalsTweets$h12500$totRatio)*100
sd13000 <- sd(TotalsTweets$h13000$totRatio)/mean(TotalsTweets$h13000$totRatio)*100
sd13500 <- sd(TotalsTweets$h13500$totRatio)/mean(TotalsTweets$h13500$totRatio)*100
sd14000 <- sd(TotalsTweets$h14000$totRatio)/mean(TotalsTweets$h14000$totRatio)*100
sd14500 <- sd(TotalsTweets$h14500$totRatio)/mean(TotalsTweets$h14500$totRatio)*100
sd15000 <- sd(TotalsTweets$h15000$totRatio)/mean(TotalsTweets$h15000$totRatio)*100
sd15500 <- sd(TotalsTweets$h15500$totRatio)/mean(TotalsTweets$h15500$totRatio)*100



sd_list = c(sd1000, sd1500, sd2000, sd2500, sd3000, 
            sd3500, sd4000, sd4500, sd5000, sd5500, sd6000, sd6500, sd7000, 
            sd7500, sd8000, sd8500, sd9000, sd9500, sd10000, sd10500, sd11000,
            sd11500, sd12000, sd12500, sd13000, sd13500, sd14000, sd14500,
            sd15000, sd15500)

HexSizes <- c(1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000, 5500, 6000, 6500, 7000, 7500, 8000, 8500,
              9000, 9500, 10000, 10500, 11000, 11500, 12000, 12500, 13000, 13500, 14000, 14500, 15000, 15500)

sd_df <- data.frame(x = HexSizes, y = sd_list)


sd_plot <- ggplot(sd_df, aes(x = HexSizes, y = sd_list)) +
  geom_line() + 
  geom_point(size = 1, color = "#dd1c77") +
  labs(x = "Hexagons' Diameter (m)", y = "sd")

sd_plot
save(hexagons, file="Chapter2/Data/hexagons.Rdata")
