
here::i_am("Chapter2/Harvey/mapping_results.R") # Declares the location of the current script

library(here)
library(leaflet)
library(htmltools)
library(ggplot2)

# Reads data
rel_levels <- readRDS(file = here("Chapter2", "Data", "rel_levels.rds")) 
SpotterInCounties <- readRDS(file = here("Chapter2", "Data", "SpotterInCounties.rds")) # 157 tweets


## MAPPING RESULTS ------------------------------------------------------
source(file = here("Chapter2", "Harvey", "addLegendCustom.R"))

pal <- colorFactor(palette = c("#1a9641", "#a6d96a", "#F4D03F", "#fd8d3c", "#ef3b2c"), 
                   domain = c("1: High", "2: High-Medium", "3: Medium","4: Low-Medium","5: Low"))

harveyClassifiedMap <- leaflet(data = rel_levels) %>% # Interactive map to see resulting clusters
  addProviderTiles(providers$Stamen.TonerLite) %>% #$Stamen.TonerLite  #CartoDB.DarkMatter
  # addRasterImage(floodR,
  #                col = '#4393c3', #
  #                opacity = 1) %>%  #0.5
  addCircleMarkers(weight = 5,
                   radius= 1.5,
                   color= ~pal(level),
                   label = ~rel_levels,
                   stroke = FALSE,
                   fill = TRUE,
                   fillOpacity = 1, #0.5
                   popup = ~htmlEscape(text)) %>%
  setView(lng = -96.5, lat = 31.5, zoom = 7.499999999999995) %>% 
  addLegendCustom(color = c("#1a9641", "#a6d96a", "#F4D03F", "#fd8d3c", "#ef3b2c"), 
                  labels = c("1: High", "2: High-Medium", "3: Medium","4: Low-Medium","5: Low"),
                  position = "bottomright",
                  title = "Relevance Level") %>%
  addScaleBar(position = c("bottomright"),
              options = scaleBarOptions())

harveyClassifiedMap

        # htmlwidgets::saveWidget(harveyClassifiedMap, file = "harveyClassifiedMap.html")


## FACET MAPS ------------------------------------------------------

library(ggspatial) # For annotation_map_til function

ggplot(data = rel_levels) +
  annotation_map_tile('cartolight') +
  geom_sf(aes(color = level),
          alpha = 0.8,
          #   colour = 'white',
          size = 0.7)+
  coord_sf(xlim = c(-100, -93.4), ylim = c(25.8, 34),
           expand = FALSE) +
  scale_color_manual(values = c("#1a9641", "#a6d96a", "#F4D03F", "#fd8d3c", "#ef3b2c")) +
  facet_wrap(~level) +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none") 


## RESULTS IN HEXAGONS ------------------------------------------------------

library(sp)
library(sf)
library(tidyverse)
library(RColorBrewer)
library(classInt)
library(USAboundaries) # To extract Texas boundary

# TexasCounties <- readRDS(file = here("Chapter2", "Data", "TexasCounties.rds")) # Total area in meters: 128295306849
# texas <- TexasCounties  %>% 
#   st_transform(crs = 4326)

TX <-us_states(resolution = "high", states = "texas")

texas <- TX  %>% 
  st_transform(crs = 4326)

texas_sp <-  as(texas, "Spatial")

sp_hex <- HexPoints2SpatialPolygons(spsample(texas_sp,
                                             n=6250, #Try with 25000 first and it was probably too small. With 12500 polygons, hex area is around 10km2
                                             type="hexagonal")) # Create hexagons based on defined number of hex

sf_hex <- st_as_sf(sp_hex) %>% 
  mutate(group = 1:nrow(.))

## Level 1 Map 

level1 <- rel_levels %>% 
  filter(level == "1: High")

# HexagonsWithTweets
hexWithLevel1 <- st_join(sf_hex, level1) %>% 
  group_by(group) %>%
  summarise(total = sum(!is.na(group)))%>% 
  mutate(totRatio = total/sum(total))# Add column that normalize totals

classes <- 4
style_method <- "fisher"
pal1 <- brewer.pal(classes, "Greens")
palData <- classIntervals(hexWithLevel1$totRatio, n = classes, style=style_method, pal = pal1)
hexWithLevel1$colores <- findColours(palData, pal1)%>%
  as.factor(.)
pal2 <- colorBin(pal1, domain = palData$brks, bins = palData$brks, pretty = FALSE)

Level1Map <- leaflet(hexWithLevel1) %>%
  setView(lng = -98.8, lat = 31, zoom = 7.5) %>%
  addProviderTiles(providers$Stamen.TonerLite) %>% 
  addPolygons(fillColor = ~colores,
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "2",
              fillOpacity = 0.8,
              popup = ~htmlEscape(sprintf("Reports per hexagon: %i",
                                          total))) %>%
  addLegend(pal = pal2,
            values = ~totRatio,
            labFormat = labelFormat(digits = 4), 
            opacity = 0.8, 
            title = "High-Relevance Tweets Density",
            position = "bottomright")
Level1Map

## Level 2 Map 

level2 <- rel_levels %>% 
  filter(level == "2: High-Medium")

# HexagonsWithTweets
hexWithLevel2 <- st_join(sf_hex, level2) %>% 
  group_by(group) %>%
  summarise(total = sum(!is.na(group)))%>% 
  mutate(totRatio = total/sum(total))# Add column that normalize totals

classes <- 4
style_method <- "fisher"
pal1 <- brewer.pal(classes, "YlGn")
palData <- classIntervals(hexWithLevel2$totRatio, n = classes, style=style_method, pal = pal1)
hexWithLevel2$colores <- findColours(palData, pal1)%>%
  as.factor(.)
pal2 <- colorBin(pal1, domain = palData$brks, bins = palData$brks, pretty = FALSE)

Level2Map <- leaflet(hexWithLevel2) %>%
  setView(lng = -98.8, lat = 31, zoom = 7.5) %>%
  addProviderTiles(providers$Stamen.TonerLite) %>% 
  addPolygons(fillColor = ~colores,
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "2",
              fillOpacity = 0.8,
              popup = ~htmlEscape(sprintf("Reports per hexagon: %i",
                                          total))) %>%
  addLegend(pal = pal2,
            values = ~totRatio,
            labFormat = labelFormat(digits = 4), 
            opacity = 0.8, 
            title = "High-Medium-Relevance Tweets Density",
            position = "bottomright")
Level2Map

## Level 3 Map 

level3 <- rel_levels %>% 
  filter(level == "3: Medium")

# HexagonsWithTweets
hexWithLevel3 <- st_join(sf_hex, level3) %>% 
  group_by(group) %>%
  summarise(total = sum(!is.na(group)))%>% 
  mutate(totRatio = total/sum(total))# Add column that normalize totals

classes <- 4
style_method <- "fisher"
pal1 <- brewer.pal(classes, "YlOrBr")
palData <- classIntervals(hexWithLevel3$totRatio, n = classes, style=style_method, pal = pal1)
hexWithLevel3$colores <- findColours(palData, pal1)%>%
  as.factor(.)
pal2 <- colorBin(pal1, domain = palData$brks, bins = palData$brks, pretty = FALSE)

Level3Map <- leaflet(hexWithLevel3) %>%
  setView(lng = -98.8, lat = 31, zoom = 7.5) %>%
  addProviderTiles(providers$Stamen.TonerLite) %>% 
  addPolygons(fillColor = ~colores,
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "2",
              fillOpacity = 0.8,
              popup = ~htmlEscape(sprintf("Reports per hexagon: %i",
                                          total))) %>%
  addLegend(pal = pal2,
            values = ~totRatio,
            labFormat = labelFormat(digits = 4),
            opacity = 0.8, 
            title = "Medium-Relevance Tweets Density",
            position = "bottomright")
Level3Map


## Level 4 Map 

level4 <- rel_levels %>% 
  filter(level == "4: Low-Medium")

# HexagonsWithTweets
hexWithLevel4 <- st_join(sf_hex, level4) %>% 
  group_by(group) %>%
  summarise(total = sum(!is.na(group)))%>% 
  mutate(totRatio = total/sum(total))# Add column that normalize totals

classes <- 4
style_method <- "fisher"
pal1 <- brewer.pal(classes, "Oranges")
palData <- classIntervals(hexWithLevel3$totRatio, n = classes, style=style_method, pal = pal1)
hexWithLevel4$colores <- findColours(palData, pal1)%>%
  as.factor(.)
pal2 <- colorBin(pal1, domain = palData$brks, bins = palData$brks, pretty = FALSE)

Level4Map <- leaflet(hexWithLevel4) %>%
  setView(lng = -98.8, lat = 31, zoom = 7.5) %>%
  addProviderTiles(providers$Stamen.TonerLite) %>% 
  addPolygons(fillColor = ~colores,
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "2",
              fillOpacity = 0.8,
              popup = ~htmlEscape(sprintf("Reports per hexagon: %i",
                                          total))) %>%
  addLegend(pal = pal2,
            values = ~totRatio,
            labFormat = labelFormat(digits = 4),
            opacity = 0.8, 
            title = "Low-Medium-Relevance Tweets Density",
            position = "bottomright")
Level4Map



## Level 5 Map 

level5 <- rel_levels %>% 
  filter(level == "5: Low")

# HexagonsWithTweets
hexWithLevel5 <- st_join(sf_hex, level5) %>% 
  group_by(group) %>%
  summarise(total = sum(!is.na(group)))%>% 
  mutate(totRatio = total/sum(total))# Add column that normalize totals

classes <- 5
style_method <- "fisher"
pal3 <- brewer.pal(classes, "Reds")
palData <- classIntervals(hexWithLevel5$totRatio, n = classes, style=style_method, pal = pal3)
hexWithLevel5$colores <- findColours(palData, pal3)%>%
  as.factor(.)
pal4 <- colorBin(pal3, domain = palData$brks, bins = palData$brks, pretty = FALSE)

Level5Map <- leaflet(hexWithLevel5) %>%
  setView(lng = -98.8, lat = 31, zoom = 7.5) %>%
  addProviderTiles(providers$Stamen.TonerLite) %>% 
  addPolygons(fillColor = ~colores,
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "2",
              fillOpacity = 0.7,
              popup = ~htmlEscape(sprintf("Reports per hexagon: %i",
                                          total))) %>%
  addLegend(pal = pal4,
            values = ~totRatio,
            labFormat = labelFormat(digits = 4), 
            opacity = 0.7, 
            title = "Low-Relevance Tweets Density",
            position = "bottomright")
Level5Map




