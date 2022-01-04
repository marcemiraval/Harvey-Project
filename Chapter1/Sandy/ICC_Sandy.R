library(tidyverse) # For ggplot. Nice plots.
library(lubridate) # Play nicely with dates
library(sf) # Spatial monster
library(scales) # for date_breaks function
# library(dplyr)

# For spatial clustering
library(dbscan)
# library(fpc) # Check if this is necessary
library(leaflet)
library(htmltools)

# For text Mining
library(tm) 
library(wordcloud)
library(tidytext)
library(stringr) # For str_to_lower

library(EWEReporting) # My package to use function to create_corpus function. 
#Install this function using devtools package

library(proxy) # To use "cosine" as method to compute distance
library(reshape2) # To use melt function
library(cluster) # For silhouette coefficient
library(lsa) # To compute cosine metric

library(topicmodels)


# Defining working directory
setwd ("Chapter1/Sandy")


####################### DATA CLEANING ###############################

# Loading data into R
sandy <- read_csv("Shelton_et_al_Sandy.csv")


sandy <- sandy %>% # Removing retweets
  filter(!str_detect(text, "^RT"))

sandy$text <- str_to_lower(sandy$text)# Converting text to lower_case letter

TweetsToExcludeSandy <- c("#sandy bgz", "#satstudytime") 

# Removing duplicated tweets and tweets created by bots
## sandy=na.omit(sandy) # Not sure what this does
# grepl function doesn't take all elements in the vector.
# So we have to paste them with an or stament like "i'm at|vegas"
sandy <- sandy[!grepl(paste(TweetsToExcludeSandy, collapse = "|"), sandy$text),] 
# sandy <- distinct(sandy, text, .keep_all = TRUE) # remove duplicate tweets



####################### DATA READING AND PROJECTING ###########################


sandy$date <- ymd_hms(sandy$created_at, tz ="UTC")

# Store tweets as simple features and project data
sandy_tweets_sf <- sandy %>% 
  select(lat = latitude, 
         lon = longitude, 
         date = date, 
         state = c_state,
         tweet = text) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% # set WGS84 as original datum
  st_transform(crs ="+proj=lcc +lat_1=20 + lat_2=60 + lat_0=40 + 
               lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +
               units=m no_defs") # Projected in North_America_Lambert_Conformal_Conic

# st_crs(sandy_tweets_sf) # Retrieve current coord ref system: EPSG: 4326 WGS84



######################### DEFINING TEMPORAL STAGES #####################################


# Creating an attribute to define event_stage for each report 
min_datetime <- min(sandy$date)
max_datetime <- max(sandy$date)

sandy$event_stage = "Calm" # Initialize Variable
sandy$event_stage[sandy$date >= min_datetime & sandy$date <
                    "2012-10-28 02:00:00 UTC"] = "PreCalm"
sandy$event_stage[sandy$date >= "2012-10-28 02:00:00 UTC" & 
                    sandy$date < "2012-10-29 02:00:00 UTC"] = "PrePeak"
sandy$event_stage[sandy$date >= "2012-10-29 02:00:00 UTC" & 
                    sandy$date < "2012-10-30 02:00:00 UTC"] = "Peak"
sandy$event_stage[sandy$date >= "2012-10-30 02:00:00 UTC" & 
                    sandy$date < "2012-10-31 02:00:00 UTC"] = "PostPeak"
sandy$event_stage[sandy$date >= "2012-10-31 02:00:00 UTC" & 
                    sandy$date < max_datetime] = "PostCalm"


######################### SPATIAL CLUSTERING ####################################

set.seed(123)

## For Sandy, I might need first to cluster spatially to the states affected

clusters <- hdbscan(sandy_tweets_sf %>% 
                      st_coordinates(), #This rounds coordinates
                    minPts = 700)

colo_clusters <- colo_tweets_sf %>% 
  mutate(cluster = clusters$cluster)

## Plotting spatial cluster results

colo_clusters <- colo_clusters %>% # Need to reproject in WGS84 datum. long lat format.
  st_transform(crs = 4326)

colo_clusters$cluster <- as.factor(colo_clusters$cluster) #Clusters as factors for coloring
pal <- colorFactor(c("#636363", "red", "Blue"), domain = c("0", "1", "2"))

# #1. or Red cluster has 608 tweets. Denver
# #2. or Blue cluster has 1998 tweets. Boulder
# 2219/4840 tweets were classified as outliers.

coloMap <- leaflet(colo_clusters) %>% # Interactive map to see resulting clusters
  addTiles()  %>%
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>% 
  addCircles(weight = 3, 
             radius=40,
             color= ~pal(cluster), 
             stroke = TRUE, 
             fillOpacity = 0.7,
             popup = ~htmlEscape(cluster))%>% 
  setView(lng = -94, lat = 40.4, zoom = 4.5)

coloMap


######################### HISTOGRAMS ###########################################


# creating frequency histograms of reports colored by stage (using create_histo function)

OutputsFile <- "/home/marcela/Coding/EWE-reporting/Sandy/Outputs/"
create_histo(InputFile = sandy, HistoColor = NA, HistoBinWidth = 3600,
             HistoName = "gen_hist_1hh", SavePath = OutputsFile)
create_histo(InputFile = sandy, HistoColor = "black", HistoBinWidth = 21600,
             HistoName = "gen_hist_6hh", SavePath = OutputsFile)


####################### CORPUSES PREPARATION ##############################################

# Preparation to use my corpus function

ToExcludeSandy <- c("http", "sandy", "fuck", "bitch","shit", "frankenstorm", "will", 
                    "going","got","dont","still","like","just","flood", "flooding",
                    "flooded","get","cant", "rt", "can", "hurricane","hurricanesandy",
                    "hurricanesandi", "lol", "sandi", "hurrican", "amp", "apocalyps")

sandy$t_text <- sandy$text # Adding t_text column
sandy <- select(sandy, -starts_with("text")) #removing text column

# Filtering out reports sent within specific stages (time intervals)
PreCalm <- subset(sandy, (event_stage == "PreCalm"))
PrePeak <- subset(sandy, (event_stage == "PrePeak"))
Peak <- subset(sandy, (event_stage == "Peak"))
PostPeak <- subset(sandy, (event_stage == "PostPeak"))
PostCalm <- subset(sandy, (event_stage == "PostCalm"))

# Using my corpus function
sandy_corpus <- create_corpus(sandy, ToExcludeSandy)
Sandy_PreCalm_corpus <- create_corpus(PreCalm, ToExcludeSandy)
Sandy_PrePeak_corpus <- create_corpus(PrePeak, ToExcludeSandy)
Sandy_Peak_corpus <- create_corpus(Peak, ToExcludeSandy)
Sandy_PostPeak_corpus <- create_corpus(PostPeak, ToExcludeSandy)
Sandy_PostCalm_corpus <- create_corpus(PostCalm, ToExcludeSandy)


############### WORDCLOUD AND DENDROGRAM FOR COMPLETE DATASET ###############################

# wordclouds and dendrogram for each stage are in separate .R files

# Constructing the Term Document Matrices and Wordclouds

dtm_sandy <-  DocumentTermMatrix(sandy_corpus)
create_wordcloud(DTMInput = dtm_sandy, sparceFactor = 0.99,
             OutFolder = OutputsFile,
             OutFile = "WordcloudAllSandy.png", background = "Black", 
             ncolors = 6, palette = "Blues")

dtm_PreCalm <-  DocumentTermMatrix(Sandy_PreCalm_corpus) 
create_wordcloud(DTMInput = dtm_PreCalm, sparceFactor = 0.999,
                 OutFolder = OutputsFile,
                 OutFile = "WordcloudSandyPreCalm.png", background = "transparent", 
                 ncolors = 8, palette = "Dark2")

dtm_PrePeak <-  DocumentTermMatrix(Sandy_PrePeak_corpus) 
create_wordcloud(DTMInput = dtm_PrePeak, sparceFactor = 0.999,
                 OutFolder = OutputsFile,
                 OutFile = "WordcloudSandyPrePeak.png", background = "transparent", 
                 ncolors = 8, palette = "Dark2")

dtm_Peak <-  DocumentTermMatrix(Sandy_Peak_corpus) 
create_wordcloud(DTMInput = dtm_Peak, sparceFactor = 0.999,
                 OutFolder = OutputsFile,
                 OutFile = "WordcloudSandyPeak.png", background = "transparent", 
                 ncolors = 8, palette = "Dark2")

dtm_PostPeak <-  DocumentTermMatrix(Sandy_PostPeak_corpus) 
create_wordcloud(DTMInput = dtm_PostPeak, sparceFactor = 0.999,
                 OutFolder = OutputsFile,
                 OutFile = "WordcloudSandyPostPeak.png", background = "transparent", 
                 ncolors = 8, palette = "Dark2")

dtm_PostCalm <-  DocumentTermMatrix(Sandy_PostCalm_corpus) 
create_wordcloud(DTMInput = dtm_Peak, sparceFactor = 0.999,
                 OutFolder = OutputsFile,
                 OutFile = "WordcloudSandyPostCalm.png", background = "transparent", 
                 ncolors = 8, palette = "Dark2")

############## HIERACHICAL CLUSTERING ################################

# Creating dendograms

create_dendogram(DTMInput = dtm_sandy, sparceFactor = 0.98, nclusters =7, 
                 OutFolder = OutputsFile,
                 OutFile = "Dendogram.png",
                 palette = "Set2")

create_dendogram(DTMInput = dtm_PreCalm, sparceFactor = 0.98, nclusters =10, 
                 OutFolder = OutputsFile,
                 OutFile = "Dendogram_PreCalm.png",
                 palette = "Set3")

create_dendogram(DTMInput = dtm_PrePeak, sparceFactor = 0.98, nclusters =8, 
                 OutFolder = OutputsFile,
                 OutFile = "Dendogram_PrePeak.png",
                 palette = "Accent")

create_dendogram(DTMInput = dtm_Peak, sparceFactor = 0.98, nclusters =7, 
                 OutFolder = OutputsFile,
                 OutFile = "Dendogram_Peak.png",
                 palette = "Accent")

create_dendogram(DTMInput = dtm_PostPeak, sparceFactor = 0.98, nclusters =10, 
                 OutFolder = OutputsFile,
                 OutFile = "Dendogram_PostPeak.png",
                 palette = "Set3")

create_dendogram(DTMInput = dtm_PostCalm, sparceFactor = 0.98, nclusters =9, 
                 OutFolder = OutputsFile,
                 OutFile = "Dendogram_PostCalm.png",
                 palette = "Set3")
