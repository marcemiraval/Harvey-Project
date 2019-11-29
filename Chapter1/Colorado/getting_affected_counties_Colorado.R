library(tigris) # Loading tigris dataset/ Census
library(sf)

# Data to create basemap
counties <- counties("Colorado", cb = FALSE, resolution = "500k")
counties_sf <- st_as_sf(counties)

affected_counties <- counties_sf %>% 
  dplyr::filter(NAME == "Adams" | NAME == "Arapahoe" | NAME == "Boulder" | 
           NAME == "Clear Creek" | NAME == "Crowley" |NAME == "Denver"|
           NAME == "El Paso" | NAME == "Fremont" | NAME == "Gilpin" |
           NAME ==  "Jefferson" | NAME ==  "Lake" | NAME ==  "Larimer" |
           NAME ==  "Lincoln" | NAME == "Logan" | NAME ==  "Morgan" | 
           NAME ==  "Sedgwick" | NAME ==  "Washington" | NAME ==  "Weld") # 18 counties were designated for public assistance (FEMA).

saveRDS(affected_counties, file = "affected_counties.rds")


