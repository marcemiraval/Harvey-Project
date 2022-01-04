library(tigris) # Loading tigris dataset/ Census
library(sf)

# Data to create basemap

affected_states <- states(cb = FALSE, resolution = "500k") %>% 
  st_as_sf() %>% 
  filter(NAME == "Maryland" | NAME == "Delaware"| 
  NAME == "New Jersey" | NAME == "New York"|
  NAME == "Connecticut" | NAME == "Massachusetts"|
  NAME == "Rhode Island"| NAME == "North Carolina"|
  NAME == "Virginia" | NAME ==  "West Virginia"|
  NAME == "Ohio"| NAME ==  "Pennsylvania"|
  NAME == "New Hampshire"   )
  
saveRDS(affected_states, file = "affected_states.rds")