library(leaflet)
library(tidyverse) # For ggplot. Nice plots.
library(sf) # Spatial monster
library(lubridate) # Play nicely with dates
library(gganimate)
library(htmltools)
library(USAboundaries) # To extract Texas boundary
library(scales) # For date_breaks function
library(OpenStreetMap)

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


######################### PREPARING ALLREPORTS DATASET ###########################################

# Adding id column to spotters
harveyNWS <- harveyNWS %>% 
  mutate(date = beginDate) %>% 
  mutate(id = row_number() + 4486) # Need to fix how to add this number in a better way

# Adding id column to tweets
TweetsHarveyTexas <- TweetsHarveyTexas %>% 
  mutate(id = row_number())

# Joining and cleaning the combined dataset
allReports <- dplyr::full_join(TweetsHarveyTexas, 
                               harveyNWS, by = "id") %>% 
  mutate(date = ymd_hms((ifelse(is.na(date.x), 
                                as.character(date.y), 
                                as.character(date.x)))), tz = "UTC") %>% 
  mutate(lon = ifelse(is.na(lon.x), lon.y, lon.x)) %>% 
  mutate(lat = ifelse(is.na(lat.x), lat.y, lat.x)) %>% 
  mutate(source = ifelse(is.na(tweet), "Validated NWS", "Twitter")) %>% 
  select(id = id,
         lon = lon,
         lat = lat,
         date = date,
         tweet = tweet,
         text = text,
         duration = duration,
         source = source)

saveRDS(allReports, file = "Data/allReports.rds")

######################### IMPORTING DATA ###########################################

# Importing Tweets
TweetsHarveyTexas_sf <- readRDS(file = "Data/TweetsHarveyTexas_sf.rds")
TweetsHarveyTexas <- readRDS(file = "Data/TweetsHarveyTexas.rds")

# Importing Spotters
HarveyNWS <- readRDS(file = "Data/HarveyNWSTexas.rds")

# Importing allReports dataset
allReports <- readRDS(file = "Data/allReports.rds")

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

ggplot(data = allReports, 
       aes(x = date, fill = source)) +
  geom_histogram(aes(y = ..count..),
                 binwidth = 10800, 
                 position="identity",
                 alpha = .7) +
  scale_x_datetime(name = "Date", 
                   breaks = date_breaks("2 day"),
                   labels = date_format("%m/%d"),
                   limits = c(as.POSIXct(min_datetimeNWS),
                              as.POSIXct(max_datetimeNWS))) + 
  scale_y_continuous(name = "Count") +
  ggtitle("") +
  theme(legend.position = "top") +
  scale_fill_manual(name = "", # No title in the legend
                    values = c("Twitter" = "#4d4d4d",
                               "Validated NWS" = "#41b6c4"),
                    aesthetics = c("colour", "fill")) 

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

basemap <- openmap(c(36.5007057189941,-106.645652770996),
                   c(25.8370609283447,-93.5078201293945),
                   minNumTiles=4) %>% # Importing OSM using Texas BB.
  openproj(projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # Asigning CRS to basemap

ani <- autoplot(basemap) +
  geom_point(data = harveyNWS, 
             mapping = aes(x = lon, 
                           y = lat, 
                           color = "red")) +
  geom_point(data = TweetsHarveyTexas, 
             mapping = aes(x = lon, 
                           y = lat, 
                           color = "blue"))+
  labs(title = "{(current_frame)}") +
  transition_manual(beginDate, cumulative = TRUE) +
  exit_fade() 

ani
 
