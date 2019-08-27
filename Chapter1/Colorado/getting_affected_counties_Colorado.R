library(tigris) # Loading tigris dataset/ Census
library(sf)

# Data to create basemap
counties <- counties("Colorado", cb = FALSE, resolution = "500k")
counties_sf <- st_as_sf(counties)
affected_counties <- counties_sf %>% 
  filter(NAME == "Arapahoe" | NAME == "Boulder" | NAME == "Denver"|
           NAME == "El Paso" | NAME ==  "Jefferson" | NAME ==  "Larimer" |
           NAME == "Logan" | NAME ==  "Morgan" | NAME ==  "Weld")

saveRDS(affected_counties, file = "affected_counties.rds")
