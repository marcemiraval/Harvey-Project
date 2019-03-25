library(leaflet)
library(tidyverse) # For ggplot. Nice plots.
library(sf) # Spatial monster
library(lubridate) # Play nicely with dates
library(gganimate)
library(htmltools)


setwd("Chapter2")

# Importing Tweets
Harvey <- readRDS(file = "Data/HarveyGeo.rds")
Harvey$date <- ymd_hms(Harvey$created, tz ="UTC")

# Store tweets as simple features and project data
harvey_sf <- Harvey %>% 
  select(lat = latitude, 
         lon = longitude, 
         date = date, 
         tweet = text) %>% 
  mutate(lat = as.double(lat)) %>% 
  mutate(lon = as.double(lon)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Importing Spotters
HarveyNWS <- readRDS(file = "Data/HarveyNWSTexas.rds")

harveyNWS_sf <- HarveyNWS %>% 
  select(lat = BEGIN_LAT, 
         lon = BEGIN_LON, 
        # date = date, 
         text = EPISODE_NARRATIVE) %>% 
  # mutate(lat = as.double(lat)) %>% 
  # mutate(lon = as.double(lon)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Interactive map to see tweets location
HarveyMap <- leaflet(harvey_sf) %>% 
  addTiles()  %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  addCircles(weight = 3, 
             radius=40,
             stroke = TRUE, 
             fillOpacity = 0.7)%>% 
  setView(lng = -94, lat = 40.4, zoom = 4.5)

HarveyMap <- leaflet() %>% 
  addTiles()  %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  addCircles(data = harveyNWS_sf, weight = 3, 
             radius=500,
             stroke = TRUE,
             color = "#FF0000",
             fillColor = "#FF0000",
             fillOpacity = 0.7,
             popup = ~htmlEscape(text)) %>% 
  addCircles(data = harvey_sf, weight = 3,
             radius=40,
             stroke = TRUE,
             fillColor = "#6754D8",
             fillOpacity = 0.7,
             popup = ~htmlEscape(tweet)) %>%
  setView(lng = -94, lat = 40.4, zoom = 4.5)

HarveyMap


file_path_name <- "Harvey/Outputs/HarveyTweetsMap.html"
htmlwidgets::saveWidget(HarveyMap, 
                        file.path(normalizePath(dirname(file_path_name)), 
                                  basename(file_path_name))) # saveWidget does not work with relative pathnames 
#and normalizePath does not work for paths to files that done exist yet.
# There is another solution for that here: https://github.com/ramnathv/htmlwidgets/issues/299

######################### HISTOGRAM ###########################################

min_datetime <- min(harvey_sf$date)
max_datetime <- max(harvey_sf$date)

ggplot(harvey_tweets_sf, aes(x = harvey_tweets_sf$date)) +
  geom_histogram(aes(y = ..count..), binwidth = 3600, 
                 position="identity", alpha =0.9)


######################### TWEETS ANIMATION ###########################################
data(state)
plot(state.name == "texas")

ggplot(data = harvey_sf, frame = date) + 
  #borders("state","texas",fill="#252525") +
  geom_sf() +
  coord_sf(crs = 4326, 
           xlim = c(-106.645652770996, -93.5078201293945), 
           ylim = c(25.8370609283447, 36.5007057189941)) +
  transition_time(date) +
  labs(title = "{round(frame_time, 0)}")
 
