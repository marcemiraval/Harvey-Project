
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


## g

library(ggmaptile)


ggplot() +
  geom_sf(data = rel_levels,
          size = 5) +
  coord_sf()


rel_levels %>%
  ggplot() +
  stat_maptiles() +
  geom_sf(aes(color = level), size = 3, show.legend = FALSE) +
  theme_void() +
  mapview()

library(ggspatial)

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



library(basemapR)
library(sf)

my_bbox <- st_bbox(rel_levels)

ggplot(data = rel_levels) +
  base_map(bbox = my_bbox, basemap = 'hydda', increase_zoom = 2) +
  geom_sf(aes(color = level),
          alpha = 0.8,
          #   colour = 'white',
          size = 0.7) +
  scale_color_manual(values = c("#1a9641", "#a6d96a", "#F4D03F", "#fd8d3c", "#ef3b2c")) +
  facet_wrap(~level) +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()) 


