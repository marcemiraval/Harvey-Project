
here::i_am("Chapter2/Harvey/mapping_results.R") # Declares the location of the current script

# Reads data
rel_levels <- readRDS(file = here("Chapter2", "Data", "rel_levels.rds")) 
SpotterInCounties <- readRDS(file = here("Chapter2", "Data", "SpotterInCounties.rds")) # 157 tweets


## MAPPING RESULTS!!! ------------------------------------------------------

harveyClassifiedMap <- leaflet() %>% # Interactive map to see resulting clusters
  addTiles()  %>%
  addProviderTiles(providers$Stamen.TonerLite) %>% #$Stamen.TonerLite  #CartoDB.DarkMatter
  # addRasterImage(floodR,
  #                col = '#4393c3', #
  #                opacity = 1) %>%  #0.5
  addCircleMarkers(data = SpotterInCounties,
                   group = "Group A",
                   weight = 5, 
                   radius = 5,
                   stroke = FALSE,
                   color = "#1a9641",
                   fill = TRUE,
                   #  fillColor = "#35978f",
                   fillOpacity = 1) %>% #With fillOpacity is less transparent #0.8
  addCircleMarkers(data = rel_levels %>% 
                     filter(level == "level2"),
                   group = "Group B",
                   weight = 5,
                   radius= 3,
                   color= "#a6d96a",
                   stroke = FALSE,
                   fill = TRUE,
                   fillOpacity = 1, #0.5
                   popup = ~htmlEscape(text)) %>%
  addCircleMarkers(data = rel_levels %>% 
                     filter(level == "level3"),
                   group = "Group C",
                   weight = 5,
                   radius= 3,
                   color= "#F4D03F",#ffffbf#ffeda0
                   stroke = FALSE,
                   fill = TRUE,
                   fillOpacity = 1, #0.5
                   popup = ~htmlEscape(text)) %>%
  addCircleMarkers(data = rel_levels %>% 
                     filter(level == "level4"),
                   group = "Group D",
                   weight = 5,
                   radius= 3,
                   color= "#fd8d3c",#fdae61
                   stroke = FALSE,
                   fill = TRUE,
                   fillOpacity = 1, #0.5
                   popup = ~htmlEscape(text)) %>%
  addCircleMarkers(data = rel_levels %>% 
                     filter(level == "level5"),
                   group = "Group E",
                   weight = 5,
                   radius= 3,
                   color= "#ef3b2c",#d7191c
                   stroke = FALSE,
                   fill = TRUE,
                   fillOpacity = 1, #0.5
                   popup = ~htmlEscape(text)) %>%
  setView(lng = -96.5, lat = 31.5, zoom = 7.499999999999995) %>% 
  addLegend(group = "Group E", 
            colors = "#ef3b2c",
            opacity = 1,
            labels = "Level 5",
            position = "bottomright") %>% 
  addLegend(group = "Group D", 
            colors = "#fd8d3c",
            opacity = 1,
            labels = "Level 4",
            position = "bottomright") %>% 
  addLegend(group = "Group C", 
            colors = "#F4D03F",
            opacity = 1,
            labels = "Level 3",
            position = "bottomright") %>% 
  addLegend(group = "Group B", 
            colors = "#a6d96a",
            opacity = 1,
            labels = "Level 2",
            position = "bottomright") %>% 
  addLegend(group = "Group A", 
            colors = "#1a9641",
            opacity = 1,
            labels = "Level 1",
            position = "bottomright") %>%
  addLayersControl(
    overlayGroups = c("Group A", "Group B", "Group C", "Group D", "Group E"),
    options = layersControlOptions(collapsed = FALSE)
  )

harveyClassifiedMap



## MAPPING RESULTS 2!!! ------------------------------------------------------

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
  addLegend(pal = pal,
            opacity = 1,
            values = ~level,
            position = "bottomright",
            title = "Relevance Level") 

harveyClassifiedMap


## MAPPING RESULTS 3!!! ------------------------------------------------------
load("addLegendCustom.R")

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
