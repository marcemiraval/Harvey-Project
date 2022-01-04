library(dplyr)
library(stringr)

setwd("Chapter2/Data")

Harvey <- readRDS(file = "Harvey.rds")

Harvey <- Harvey %>% # Removing retweets
  filter(!str_detect(text, "^RT"))

Harvey <- HarveyFull %>% 
  select(favoriteCount, created, id, retweetCount, longitude, latitude, text)

HHarveyPart2.7_df <- readRDS(file = "HHarveyPart2.7_df.rds")

HarveyFull <- HarveyFull %>%
  bind_rows(HHarveyPart2.7_df)

saveRDS(Harvey, file = "Harvey.rds")


