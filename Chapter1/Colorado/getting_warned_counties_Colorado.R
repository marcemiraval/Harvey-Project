library(tigris) # Loading tigris dataset/ Census
library(sf)

# This code will need ot be updated next month with the new update of the tigris package. So far using the github version

#options(tigris_use_cache = TRUE)

# Data to create basemap
counties <- counties("Colorado", cb = FALSE, resolution = "500k")
counties_sf <- st_as_sf(counties)

warned_counties <- counties_sf %>% 
  dplyr::filter(NAME == "Adams" | NAME == "Arapahoe" | NAME == "Boulder" |  NAME == "Broomfield" |
           NAME == "Clear Creek" | NAME == "Denver"| NAME == "Douglas"|
           NAME == "El Paso" | NAME == "Elbert" | NAME == "Fremont" | NAME == "Gilpin" |
           NAME ==  "Jefferson" | NAME ==  "Larimer" | NAME ==  "Lincoln" | NAME ==  "Logan" |
           NAME ==  "Morgan" | NAME ==  "Park" | NAME == "Pueblo" |
           NAME ==  "Sedgwick" | NAME ==  "Washington" | NAME ==  "Weld") 
# 21 counties warned by NWS according to this report: http://www.nws.noaa.gov/om/assessments/pdfs/14colorado_floods.pdf
# which is also in Mendeley

saveRDS(warned_counties, file = "Chapter1/Colorado/Data/warned_counties.rds")


