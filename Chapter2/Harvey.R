library(leaflet)
library(tidyverse) # For ggplot. Nice plots.
library(sf) # Spatial monster
library(lubridate) # Play nicely with dates


setwd("Chapter2")
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


HarveyMap <- leaflet(harvey_sf) %>% # Interactive map to see tweets location
  addTiles()  %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  addCircles(weight = 3, 
             radius=40,
             stroke = TRUE, 
             fillOpacity = 0.7)%>% 
  setView(lng = -94, lat = 40.4, zoom = 4.5)

HarveyMap


ggplot(harvey_sf, frame= date) +
  borders("state","maryland",fill="#252525") + 
  geom_sf() + 
  stat_sf_coordinates()


ggplot(sandy_east, aes(x=x, y=y, color = "grey",
                       frame=created_at))+
  borders("state","maryland",fill="#252525")+
  borders("state","delaware",fill="#252525")+
  borders("state","new jersey",fill="#252525")+
  borders("state","new york",fill="#252525")+
  borders("state","connecticut",fill="#252525")+
  borders("state","massachusetts",fill="#252525")+
  borders("state","rhode island",fill="#252525")+
  borders("state","north carolina",fill="#252525")+
  borders("state","virginia",fill="#252525")+
  borders("state","west virginia",fill="#252525")+
  borders("state","ohio",fill="#252525")+
  borders("state","new hampshire",fill="#252525")+
  borders("state","pennsylvania",fill="#252525")+
  geom_point(size=0.2)+
  coord_map("lambert", lat0=20, lat=50)+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_rect(fill="black"),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_rect(fill="black"),
        plot.title = element_text(family="Garamond", hjust=0.5,lineheight=.8, 
                                  colour = "white",size=16)) +
  transition_time(created_at) +
  labs(title = "{round(frame_time, 0)}")