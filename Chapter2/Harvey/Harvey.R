library(leaflet)
library(tidyverse) # For ggplot. Nice plots.
library(sf) # Spatial monster
library(lubridate) # Play nicely with dates
library(gganimate)
library(htmltools)
library(USAboundaries) # To extract Texas boundary

setwd("Chapter2")


######################### IMPORTING DATA ###########################################

# Importing Tweets
Harvey <- readRDS(file = "Data/HarveyGeo.rds")

# Importing Spotters
HarveyNWS <- readRDS(file = "Data/HarveyNWSTexas.rds")


######################### PREPARING TWITTER DATA ###########################################

# Filtering and formating
harvey <- Harvey %>% 
  select(lat = latitude, 
         lon = longitude, 
         date = created, 
         tweet = text) %>% 
  mutate(lat = as.double(lat)) %>% 
  mutate(lon = as.double(lon)) %>% 
  mutate(date = ymd_hms(date, tz ="UTC"))

# Store tweets as simple features and project data
harvey_sf <- harvey %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)


# Storing datasets containing Tweets only for Texas in sf and df format
TX <-us_states(resolution = "high", states = "texas")
st_crs(TX)
TweetsHarveyTexas_sf <- st_intersection(harvey_sf, TX) # warning is ok

TweetsHarveyTexas <- TweetsHarveyTexas_sf %>% 
  st_set_geometry(NULL) %>%
  mutate(lat = unlist(map(TweetsHarveyTexas_sf$geometry,1)),
         long = unlist(map(TweetsHarveyTexas_sf$geometry,2)))

saveRDS(TweetsHarveyTexas_sf, file = "Data/TweetsHarveyTexas_sf.rds")
saveRDS(TweetsHarveyTexas, file = "Data/TweetsHarveyTexas.rds")


######################### IMPORTING DATA ###########################################

# Importing Tweets
TweetsHarveyTexas_sf <- readRDS(file = "Data/TweetsHarveyTexas_sf.rds")
TweetsHarveyTexas <- readRDS(file = "Data/TweetsHarveyTexas.rds")

######################### PREPARING NWS DATA ###########################################

harveyNWS_sf <- HarveyNWS %>% 
  select(lat = BEGIN_LAT, 
         lon = BEGIN_LON, 
         beginDate = BEGIN_DATE_TIME, 
         endDate = END_DATE_TIME,
         text = EPISODE_NARRATIVE) %>% 
  mutate(beginDate = ymd_hms(beginDate, tz = "UTC")) %>% 
  mutate(endDate = ymd_hms(endDate, tz = "UTC")) %>% 
  mutate(duration = abs(difftime(beginDate, endDate, units = "secs")))


%>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) 


######################### HISTOGRAM ###########################################

# Histogram for all tweets
min_datetime <- min(harvey_sf$date) 
max_datetime <- max(harvey_sf$date)

ggplot(harvey_sf, aes(x = harvey_sf$date)) +
  geom_histogram(aes(y = ..count..), binwidth = 3600, 
                 position="identity", alpha =0.9)

# Histogram for tweets only in Texas
min_datetime <- min(TweetsHarveyTexas$date)
max_datetime <- max(TweetsHarveyTexas$date)

ggplot(TweetsHarveyTexas, aes(x = TweetsHarveyTexas$date)) +
  geom_histogram(aes(y = ..count..), binwidth = 3600, 
                 position="identity", alpha =0.9)

# Histogram for NWS Reports




######################### INTERACTIVE MAP ###########################################

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



######################### TWEETS ANIMATION ###########################################
data(state)

ggplot(data = TweetsHarveyTexas, aes(frame = date, cumulative = TRUE)) + 
  borders("state","texas",fill="#bdbdbd") +
  geom_sf() +
  coord_sf(crs = 4326) +
  transition_time(date) +
  labs(title = "{round(frame_time, 0)}")
 
