library(tidyverse) # For ggplot. Nice plots.
library(lubridate) # Play nicely with dates
library(sf) # Spatial monster
library(scales) # for date_breaks function
library(tidytext)
library(dplyr)
library(stringr)
library(dbscan)
library(fpc)


library(EWEReporting) # My package to use function to create_corpus function. 
#Install this function using devtools package

library(tm) # For text Mining
library(wordcloud)


library(lsa) # To compute cosine metric
library(ggdendro)





# Defining working directory
setwd ("Chapter1/Colorado")


####################### DATA CLEANING ###############################

# Loading data into R
colorado <- read.csv("ColoradoFloodOriginal.csv", header = TRUE, sep = ",")

# Removing duplicated tweets and tweets created by bots
colorado=na.omit(colorado)
colorado$t_text <- str_to_lower(colorado$t_text) # Converting text to lower_case letter
TweetsToExclude <- c("i'm at", "vegas", "#job", "tweetmyjobs", "game","shirts",
                     "i like that b", "playboy ranks", "it's a rock","bear","hike","football")

# grepl function doesn't take all elements in the vector.
# So we have to paste them with an or stament like "i'm at|vegas"
colorado <- colorado[!grepl(paste(TweetsToExclude, collapse = "|"), colorado$t_text),]
colorado <- distinct(colorado, t_text, .keep_all = TRUE) # remove duplicate tweets

####################### DATA READING, CLEANING AND PROJECTING ###########################

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

# Haiyun's stages 
# colo_tweets_sf$flood_stage[colo_tweets_sf$date >= min_datetime & 
#                              colo_tweets_sf$date < "2013-09-09 00:00:00 PDT"] = "Pre_flood"
# colo_tweets_sf$flood_stage[colo_tweets_sf$date >= "2013-09-09 00:00:00 PDT" & 
#                              colo_tweets_sf$date < "2013-09-16 00:00:00 PDT"] = "Flood"
# colo_tweets_sf$flood_stage[colo_tweets_sf$date >= "2013-09-16 00:00:00 PDT" &
#                              colo_tweets_sf$date < "2013-09-23 00:00:00 PDT"] = "Immediate_Aftermath"
# colo_tweets_sf$flood_stage[colo_tweets_sf$date >= "2013-09-23 00:00:00 PDT" &
#                              colo_tweets_sf$date <= max_datetime] = "Post_Flood"

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

######################### HISTOGRAMS ###########################################

## Making sure stages are defined as categorical variables
colo_tweets_sf$flood_stage <- as.factor(colo_tweets_sf$flood_stage)
# levels(colo_tweets_sf$flood_stage) # Checking levels and seeing order
colo_tweets_sf$flood_stage <- factor(colo_tweets_sf$flood_stage,
                                     levels(colo_tweets_sf$flood_stage) [c(4,1,2,3)])#reorder factor levels for legend


# creating frequency histograms of reports colored by stage (using create_histo function)
OutputsFile <- "Outputs/mystages"
create_histo(InputFile = colo_tweets_sf, HistoColor = NA, HistoBinWidth = 3600,
             HistoName = "gen_hist_1h", SavePath = OutputsFile)
create_histo(InputFile = colorado, HistoColor = "black", HistoBinWidth = 21600, 
             HistoName = "gen_hist_6h", SavePath = OutputsFile)#Not working. Check if necessary


####################### CORPUSES PREPARATION #################################################

# The list of valid options
stages <<- list("Pre_flood", "Flood", "Immediate_Aftermath", "Post_Flood") # To use when I create function

# Filtering out reports sent within specific stages (time intervals)
# preFlood <- subset(colorado, (flood_stage == "Pre_flood"))
# flood <- subset(colorado, (flood_stage == "Flood"))
# iAftermath <- subset(colorado, (flood_stage == "Immediate_Aftermath"))
# postFlood <- subset(colorado, (flood_stage == "Post_Flood"))

# Using my corpus function
ToExclude <- c("boulderflood", "boulder", "mdt", "#colorado", "#coflood", "like",
               "will","septemb", "just", "amp", "colo", "love", "can", "one", "stay", "get",
               "safe", "see", "look", "morn", "issu", "dont", "lol", "#boulder", "im", "cu", 
               "#coloradostrong", "#cofloodrelief", "@noblebrett", "rt", "#cowx", "ill", "@stapletondenver",
               "september", "@dailycamera", "colorado", "@boulderflood", "#boulderflood", "youre", "flood", "flooding", "floods")


# From here on we will be tidytexting

colo_tweets <- colo_tweets_sf %>%
#  filter(flood_stage == "Flood") %>% 
  st_set_geometry(NULL) %>% 
  select(tweet) %>% 
  rename(text = `tweet`) 
## Still need to look for a way to delete retweets
  
colo_tokens <- colo_tweets %>%
  unnest_tokens(word, text, "tweets") %>% ## It seems better to use the specific argument to unnest tokens in tweets
  filter(!str_detect(word, "[0-9]"), !word %in% ToExclude)%>% 
  anti_join(stop_words)%>%
 # mutate(word = wordStem(word))%>%
  count(word, sort = TRUE) 

# define a nice color palette
pal <- brewer.pal(8,"Dark2")

# plot the 50 most common words
colo_tokens %>% 
  with(wordcloud(word, n, scale=c(4,0.5),
                 min.freq = 7, max.words = 30,
                 random.order = FALSE, 
                 rot.per = FALSE, 
                 use.r.layout = FALSE,
                 color = brewer.pal(5, "Blues"),
                 family = "Helvetica"))


# Check how I created wordclouds for the shiny app in the server file.
# It seems that create_corpus is unnecesary

# colorado_corpus <- create_corpus(colo_tokens, ToExclude)
# preco_corpus <- create_corpus(preFlood, ToExclude)
# floodco_corpus <- create_corpus(flood, ToExclude)
# iaco_corpus <- create_corpus(iAftermath, ToExclude)
# post_co_corpus <- create_corpus(postFlood, ToExclude)

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
