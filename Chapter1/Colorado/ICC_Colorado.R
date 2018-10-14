library(tidyverse) # For ggplot. Nice plots.
library(lubridate) # Play nicely with dates
library(sf) # Spatial monster
library(scales) # for date_breaks function
library(dplyr)

# For spatial clustering
library(dbscan)
library(fpc) # Check if this is necessary
library(leaflet)
library(htmltools)

# For text Mining
library(tm) 
library(wordcloud)
library(tidytext)
library(stringr) # For str_to_lower

library(EWEReporting) # My package to use function to create_corpus function. 
#Install this function using devtools package




library(lsa) # To compute cosine metric
library(ggdendro)





# Defining working directory
setwd ("Chapter1/Colorado")


####################### DATA CLEANING ###############################

# Loading data into R
colorado <- read_csv("ColoradoFloodOriginal.csv")

colorado <- colorado %>% # Removing retweets
  filter(!str_detect(t_text, "^RT"))

colorado$t_text <- str_to_lower(colorado$t_text) # Converting text to lower_case letter

TweetsToExclude <- c("i'm at", "vegas", "#job", "tweetmyjobs", "game","shirts",
                     "i like that b", "playboy ranks", "it's a rock","bear","hike","football")

# Here is to remove tweets from bots
# grepl function doesn't take all elements in the vector.
# So we have to paste them with an or stament like "i'm at|vegas"
colorado <- colorado[!grepl(paste(TweetsToExclude, collapse = "|"), colorado$t_text),] 


####################### DATA READING AND PROJECTING ###########################


# Format date/time
colorado$date <- ymd_hms(colorado$created_at, tz="UTC")

# Store tweets as simple features and project data
colo_tweets_sf <- colorado %>% 
  select(lat = latitude, 
         lon = longitude, 
         date = date, 
         state = c_state,
         tweet = t_text) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% # set WGS84 as original datum
  st_transform(crs ="+proj=lcc +lat_1=20 + lat_2=60 + lat_0=40 + 
               lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +
               units=m no_defs") # Projected in North_America_Lambert_Conformal_Conic

# st_crs(colo_tweets_sf) # Retrieve current coord ref system: EPSG: 4326 WGS84

######################### DEFINING TEMPORAL STAGES #####################################

# Creating an attribute to define flood_stage for each report
min_datetime <- "2013-09-01 00:00:48 PDT"
max_datetime <- "2013-09-30 23:52:05 PDT"

colo_tweets_sf$flood_stage = "Pre_flood" # Initialize Variable

# Ye's stages 
# colo_tweets_sf$flood_stage[colo_tweets_sf$date >= min_datetime & 
#                              colo_tweets_sf$date < "2013-09-09 00:00:00 PDT"] = "Pre_flood"
# colo_tweets_sf$flood_stage[colo_tweets_sf$date >= "2013-09-09 00:00:00 PDT" & 
#                              colo_tweets_sf$date < "2013-09-16 00:00:00 PDT"] = "Flood"
# And the Inmmediate aftermath and post_flood are the same

# My stages
colo_tweets_sf$flood_stage[colo_tweets_sf$date >= min_datetime & 
                             colo_tweets_sf$date < "2013-09-11 00:00:00 PDT"] = "Pre_flood"
colo_tweets_sf$flood_stage[colo_tweets_sf$date >= "2013-09-11 00:00:00 PDT" & 
                             colo_tweets_sf$date < "2013-09-16 00:00:00 PDT"] = "Flood"
colo_tweets_sf$flood_stage[colo_tweets_sf$date >= "2013-09-16 00:00:00 PDT" &
                             colo_tweets_sf$date < "2013-09-23 00:00:00 PDT"] = "Immediate_Aftermath"
colo_tweets_sf$flood_stage[colo_tweets_sf$date >= "2013-09-23 00:00:00 PDT" &
                             colo_tweets_sf$date <= max_datetime] = "Post_Flood"


######################### SPATIAL CLUSTERING ####################################

set.seed(123)

clusters <- hdbscan(colo_tweets_sf %>%
                      st_coordinates(), #This rounds coordinates
                    minPts = 350)

colo_clusters <- colo_tweets_sf %>% 
  mutate(cluster = clusters$cluster)

## Plotting spatial cluster results

colo_clusters_wgs84 <- colo_clusters %>% # Need to reproject in WGS84 datum. long lat format.
  st_transform(crs = 4326)

colo_clusters_wgs84$cluster <- as.factor(colo_clusters_wgs84$cluster) #Clusters as factors for coloring
pal <- colorFactor(c("#636363", "red", "Blue"), domain = c("0", "1", "2"))

# #1. or Red cluster has 608 tweets. Denver
# #2. or Blue cluster has 1998 tweets. Boulder
# 2219/4840 tweets were classified as outliers.

coloMap <- leaflet(colo_clusters_wgs84) %>% # Interactive map to see resulting clusters
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


######################### HISTOGRAM ###########################################

## Making sure stages are defined as categorical variables
colo_tweets_sf$flood_stage <- as.factor(colo_tweets_sf$flood_stage)
# levels(colo_tweets_sf$flood_stage) # Checking levels and seeing order
colo_tweets_sf$flood_stage <- factor(colo_tweets_sf$flood_stage,
                                     levels(colo_tweets_sf$flood_stage) [c(4,1,2,3)])#reorder factor levels for legend


# creating frequency histograms of reports colored by stage (using create_histo function)
create_histo(InputFile = colo_tweets_sf, HistoColor = NA, HistoBinWidth = 3600,
             HistoName = "gen_hist_1h", SavePath = "Outputs")


####################### CORPUSES PREPARATION AND WORDCLOUDS #################################################

# The list of valid options
stages <<- list("Pre_flood", "Flood", "Immediate_Aftermath", "Post_Flood") # To use when I create function

# Using my corpus function
ToExclude <- c("boulderflood", "boulder", "mdt", "#colorado", "#coflood", "like",
               "will","septemb", "just", "amp", "colo", "love", "can", "one", "stay", "get",
               "safe", "see", "look", "morn", "issu", "dont", "lol", "#boulder", "im", "cu", 
               "#coloradostrong", "#cofloodrelief", "@noblebrett", "rt", "#cowx", "ill", "@stapletondenver",
               "september", "@dailycamera", "colorado", "@boulderflood", "#boulderflood", "youre", "flood", 
               "flooding", "floods", "flooded")


# From here on we will be tidytexting

setwd("Outputs/mystages")

create_wordcloud <- function(stage){
  
  #names(stage)
  #png(filename = names(stage), width=3, height=3, units="in", res=300, bg = "transparent") 
  
  colo_tweets <- colo_clusters_wgs84 %>%
    filter(cluster == "1" | cluster == "2") %>% 
    st_set_geometry(NULL) %>% 
    select(tweet) %>% 
    rename(text = `tweet`) 
  
  colo_tokens <- colo_tweets %>%
    unnest_tokens(word, text, "tweets") %>% ## It seems better to use the specific argument to unnest tokens in tweets
    filter(!str_detect(word, "[0-9]"), !str_detect(word, "#"), !word %in% ToExclude)%>% 
    anti_join(stop_words)%>%
    # mutate(word = wordStem(word))%>%
    count(word, sort = TRUE) 
  
  # define color palette
  pal <- brewer.pal(8,"Dark2")
  
  # plot the 30 most common words
  colo_tokens %>% 
    with(wordcloud(word, n, scale=c(4,0.5),
                   min.freq = 7, max.words = 30,
                   random.order = FALSE, 
                   rot.per = FALSE, 
                   use.r.layout = FALSE,
                   color = brewer.pal(5, "Blues"),
                   family = "Helvetica"))
}

lapply(stages, create_wordcloud)

####################### WORDCLOUD AND DENDROGRAM FOR COMPLETE DATASET ###############################

# wordclouds and dendrogram for each stage are in separate .R files

# Constructing the Term Document Matrices and Wordclouds

# Histogram including all reports
dtm_colorado <-  DocumentTermMatrix(colorado_corpus)
create_wordcloud(DTMInput = dtm_colorado, sparceFactor = 0.999999999999,
                 OutFolder = OutputsFile,
                 OutFile = "WordcloudAllColorado.png", background = "White", 
                 ncolors = 6, palette = "YlGnBu")

dtm_colpre <-  DocumentTermMatrix(preco_corpus)
create_wordcloud(DTMInput = dtm_colpre, sparceFactor = 0.999999999999,
                 OutFolder = OutputsFile,
                 OutFile = "WdPreFlood.png", background = "transparent", 
                 ncolors = 6, palette = "Dark2")

dtm_colFlood <-  DocumentTermMatrix(floodco_corpus)
create_wordcloud(DTMInput = dtm_colFlood, sparceFactor = 0.999999999999,
                 OutFolder = OutputsFile,
                 OutFile = "WdFlood.png", background = "transparent", 
                 ncolors = 8, palette = "Dark2")

dtm_col_ia <-  DocumentTermMatrix(iaco_corpus)
create_wordcloud(DTMInput = dtm_col_ia, sparceFactor = 0.999999999999,
                 OutFolder = OutputsFile,
                 OutFile = "WdImmediateAfter.png", background = "transparent", 
                 ncolors = 6, palette = "Dark2")

dtm_col_post <-  DocumentTermMatrix(post_co_corpus)
create_wordcloud(DTMInput = dtm_col_post, sparceFactor = 0.999999999999,
                 OutFolder = OutputsFile,
                 OutFile = "WdPostFlood.png", background = "transparent", 
                 ncolors = 6, palette = "Dark2")


############## HIERACHICAL CLUSTERING ################################

# Creating dendograms

create_dendogram(DTMInput = dtm_colorado , sparceFactor = 0.98, nclusters = 6, 
                 OutFolder = OutputsFile,
                 OutFile = "Dendogram.png",
                 palette = "Accent")

create_dendogram(DTMInput = dtm_colpre, sparceFactor = 0.98, nclusters = 7, 
                 OutFolder = OutputsFile,
                 OutFile = "Dendogram_colpre.png",
                 palette = "Set2")

create_dendogram(DTMInput = dtm_colFlood, sparceFactor = 0.98, nclusters = 6, 
                 OutFolder = OutputsFile,
                 OutFile = "Dendogram_colFlood.png",
                 palette = "Accent")

create_dendogram(DTMInput = dtm_col_ia, sparceFactor = 0.98, nclusters = 9, 
                 OutFolder = OutputsFile,
                 OutFile = "Dendogram_col_ia.png",
                 palette = "Set3")

create_dendogram(DTMInput = dtm_col_post, sparceFactor = 0.98, nclusters = 9, 
                 OutFolder = OutputsFile,
                 OutFile = "Dendogram_col_post.png",
                 palette = "Set3")
