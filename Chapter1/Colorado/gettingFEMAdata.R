# install.packages("devtools")
remotes::install_github("mikejohnson51/NFHL")
remotes::install_github("mikejohnson51/AOI")

# load libraries

library(NFHL)
library(AOI)
library(dplyr)
library(mapview)

# Define an AOI around UCSB
AOI <- aoi_get(state = "CO")
mapview(AOI)

# Look at layerID 28 description
nfhl_describe(0)$Description
#> [1] "Labels for flood zones. "

# Extract Flood Hazard Polygons and filter
floodhazard <- nfhl_get(AOI, 0)


%>% filter(SFHA_TF == "T")

mapview(floodhazard)
plot(floodhazard)

