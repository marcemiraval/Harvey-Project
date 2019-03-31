library(leaflet)
library(tidyverse) # For ggplot. Nice plots.
library(sf) # Spatial monster
library(lubridate) # Play nicely with dates
library(gganimate)
library(htmltools)
library(USAboundaries) # To extract Texas boundary
library(scales) # For date_breaks function

setwd("Chapter2")


######################### IMPORTING US TWITTER DATA ###########################################

# Importing Tweets
Harvey <- readRDS(file = "Data/HarveyGeo.rds")

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
  mutate(lon = unlist(map(TweetsHarveyTexas_sf$geometry,1)),
         lat = unlist(map(TweetsHarveyTexas_sf$geometry,2)))


saveRDS(TweetsHarveyTexas_sf, file = "Data/TweetsHarveyTexas_sf.rds")
saveRDS(TweetsHarveyTexas, file = "Data/TweetsHarveyTexas.rds")


######################### IMPORTING DATA ###########################################

# Importing Tweets
TweetsHarveyTexas_sf <- readRDS(file = "Data/TweetsHarveyTexas_sf.rds")
TweetsHarveyTexas <- readRDS(file = "Data/TweetsHarveyTexas.rds")

# Importing Spotters
HarveyNWS <- readRDS(file = "Data/HarveyNWSTexas.rds")

######################### PREPARING NWS DATA ###########################################

harveyNWS <- HarveyNWS %>% 
  select(lat = BEGIN_LAT, 
         lon = BEGIN_LON, 
         beginDate = BEGIN_DATE_TIME, 
         endDate = END_DATE_TIME,
         text = EPISODE_NARRATIVE) %>% 
  mutate(beginDate = dmy_hms(beginDate, tz = "UTC")) %>% 
  mutate(endDate = dmy_hms(endDate, tz = "UTC")) %>% 
  mutate(duration = abs(difftime(beginDate, endDate, units = "secs")))


harveyNWS_sf <- harveyNWS %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) 


######################### HISTOGRAM ###########################################

# Min and Max values for tweets only in Texas
min_datetime <- min(TweetsHarveyTexas$date)
max_datetime <- max(TweetsHarveyTexas$date)

# Min and Max values for NWS Reports
min_datetimeNWS <- min(harveyNWS$beginDate)
max_datetimeNWS <- max(harveyNWS$beginDate)


## Histogram for NWS Reports and Tweets

ggplot() +
  geom_histogram(data = TweetsHarveyTexas,
                 aes(x = TweetsHarveyTexas$date, y = ..count..), 
                 fill = "#4d4d4d",
                 binwidth = 10800, 
                 position="identity", alpha =0.9) +
  geom_histogram(data = harveyNWS, 
                 aes(x = harveyNWS$beginDate, y = ..count..), 
                 fill = "#41b6c4", 
                 binwidth = 10800, 
                 position="identity", 
                 alpha =0.9) +
  scale_x_datetime(name = "Date", 
                   breaks = date_breaks("2 day"),
                   labels = date_format("%m/%d"),
                   limits = c(as.POSIXct(min_datetimeNWS),
                              as.POSIXct(max_datetimeNWS))) + 
  scale_y_continuous(name = "Count") +
  ggtitle("") +
  theme(legend.position = "top") +
  guides(color = "none")

ggsave("Harvey/Outputs/JointHistogram.png", width = 9, height = 5)

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
 
