library(tidyverse) # For ggplot. Nice plots.


setwd("Coding/MarcesThesis/Chapter2/Data")

# Loading data into R
HarveyNWS <- read_csv("Data/StormEvents_details-ftp_v1.0_d2017_c20181219.csv")

HarveyNWSAugustTexas <- HarveyNWS %>% 
  filter(BEGIN_YEARMONTH == "201708") %>% 
  filter(BEGIN_DAY > 18) %>% 
  filter(STATE == "TEXAS")

HarveyNWSSepTexas <- HarveyNWS %>% 
  filter(BEGIN_YEARMONTH == "201709") %>% 
  filter(BEGIN_DAY < 23) %>% 
  filter(STATE == "TEXAS") 

HarveyNWSTexas <- dplyr::bind_rows(HarveyNWSAugustTexas, HarveyNWSSepTexas) %>% 
  filter(!is.na(BEGIN_LAT))

saveRDS(HarveyNWSTexas, file = "Data/HarveyNWSTexas.rds")

