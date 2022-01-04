library(sf)
library(dplyr)


nc <- st_read(system.file("shape/nc.shp", package="sf"))
nc <- st_transform(nc, 2264)
nc[1,] # prints the first record
plot(nc)


nc %>% select(NWBIR74) %>% head(2) # showing how methods from tydiverse/dplyr work
nc %>% as.data.frame %>% select(NWBIR74) %>% head(2) # If we want to drop geometry

nc[1, "NWBIR74"] #We can subset feature sets by using the square bracket notation
nc[1, "NWBIR74", drop = TRUE] #use the drop argument to drop geometries

Ashe = nc[nc$NAME == "Ashe",] #we can also use a spatial object as the row selector, to select features that intersect with another spatial feature
class(Ashe)
nc[Ashe,] # in the result set Ashe is included, as the default value for argument op in [.sf is st_intersects, and Ashe intersects with itself. 

Ashe = nc[nc$NAME == "Ashe",]
nc[Ashe, op = st_touches] #We could exclude self-intersection by using predicate st_touches (overlapping features don’t touch)


nc %>% filter(lengths(st_touches(., Ashe)) > 0)

a <- sf_hex %>% st_union(.,sandySpot_sf) %>% 
  group_by(group) %>% 
  summarise(sandySpot_sf = count(sandySpot_sf))
#Or maybe st_contains??