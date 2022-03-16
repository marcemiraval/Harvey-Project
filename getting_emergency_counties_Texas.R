library(tigris) # Loading tigris dataset/ Census
library(sf)

options(tigris_use_cache = TRUE)

# Data to create basemap
counties <- counties("Texas", cb = FALSE, resolution = "500k")
counties_sf <- st_as_sf(counties)

TexasCounties <- counties_sf %>% 
  dplyr::filter(NAME == "Angelina" | NAME == "Aransas" | NAME == "Atascosa" | NAME == "Austin" | NAME == "Bastrop" |NAME == "Bee"|
           NAME == "Bexar" | NAME == "Brazoria" | NAME == "Brazos" | NAME ==  "Burleson" | NAME ==  "Caldwell" | NAME ==  "Calhoun" |
           NAME == "Cameron" | NAME == "Chambers" | NAME == "Colorado" | NAME ==  "Comal" | NAME ==  "DeWitt" | NAME ==  "Fayette" |
           NAME == "Fort Bend" | NAME == "Galveston" | NAME == "Goliad" | NAME ==  "Gonzales" | NAME ==  "Grimes" | NAME ==  "Guadalupe" |
           NAME == "Harris" | NAME == "Hardin" | NAME == "Jackson" | NAME ==  "Jasper" | NAME ==  "Jefferson" | NAME ==  "Jim Wells" |
           NAME == "Karnes" | NAME == "Kerr" | NAME == "Kleberg" | NAME ==  "Lavaca" | NAME ==  "Lee" | NAME ==  "Leon" |
           NAME == "Liberty" | NAME == "Live Oak" | NAME == "Madison" | NAME ==  "Matagorda" | NAME ==  "Milam" | NAME ==  "Montgomery" |
           NAME == "Newton" | NAME == "Nueces" | NAME == "Orange" | NAME ==  "Polk" | NAME ==  "Refugio" | NAME ==  "Sabine" |
           NAME == "San Augustine" | NAME == "San Jacinto" | NAME == "San Patricio" | NAME ==  "Trinity" | NAME ==  "Tyler" | NAME ==  "Victoria" |
           NAME ==  "Waller" | NAME == "Walker" | NAME ==  "Washington" | NAME ==  "Wharton" | NAME ==  "Willacy" | NAME ==  "Wilson") # 18 counties were designated for public assistance (FEMA).

TexasCountiesA <- sum(TexasCounties$ALAND) #Total area = 128295 Km2 

saveRDS(TexasCounties, file = "Chapter2/Data/TexasCounties.rds")


