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






# Defining working directory
setwd ("Chapter1/Colorado")


####################### DATA CLEANING ###############################

# Loading data into R
colorado <- read_csv("ColoradoFloodOriginal.csv")

colorado <- colorado %>% # Removing retweets
  filter(!str_detect(t_text, "^RT"))

colorado$t_text <- str_to_lower(colorado$t_text) # Converting text to lower_case letter

TweetsToExclude <- c("i'm at", "vegas", "#job", "tweetmyjobs",
                     "i like that b", "playboy ranks")


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


######################### HISTOGRAM ###########################################

## Making sure stages are defined as categorical variables
colo_clusters$flood_stage <- as.factor(colo_clusters$flood_stage)
# levels(colo_clusters$flood_stage) # Checking levels and seeing order
colo_clusters$flood_stage <- factor(colo_clusters$flood_stage,
                                     levels(colo_clusters$flood_stage) [c(4,1,2,3)])
#reorder factor levels for legend


# creating frequency histograms of reports colored by stage (using create_histo function)
create_histo(InputFile = colo_clusters, HistoColor = NA, HistoBinWidth = 3600,
             HistoName = "gen_hist_1h", SavePath = "Outputs")

colo_cluster_denver <- colo_clusters %>% 
  filter(cluster == "1")
  
create_histo(InputFile = colo_cluster_denver, HistoColor = NA, HistoBinWidth = 3600,
             HistoName = "Denver_hist_1h", SavePath = "Outputs")

colo_cluster_boulder <- colo_clusters %>% 
  filter(cluster == "2")

create_histo(InputFile = colo_cluster_boulder, HistoColor = NA, HistoBinWidth = 3600,
             HistoName = "Boulder_hist_1h", SavePath = "Outputs")


####################### WORDCLOUDS #################################################

# The list of valid options
stages <- list("Pre_flood", "Flood", "Immediate_Aftermath", "Post_Flood") # To use when I create function

# Using my corpus function
ToExclude <- c("boulderflood", "boulder", "mdt", "#colorado", "#coflood", "like",
               "will","septemb", "amp", "lol", "#boulder", "im", "ive", "#coloradostrong", 
               "#cofloodrelief", "@noblebrett", "rt", "#cowx", "ill", "@stapletondenver",
               "september", "@dailycamera", "colorado", "@boulderflood", "#boulderflood", 
               "youre", "flood", "flooding", "floods", "flooded", "colo", "cu", "denver", "county")


create_wordcloud <- function(stage){

  colo_tweets <- colo_clusters %>%
    # filter(cluster == "1" | cluster == "2") %>%
    filter(cluster == "1") %>%
    filter(flood_stage == stage) %>% 
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

############## HIERACHICAL CLUSTERING ################################

cosine_dist <- function(stage, clusterID){
  
  colo_tweets <- colo_clusters %>% 
    filter(cluster == clusterID) %>%
    filter(flood_stage == stage) %>%
    st_set_geometry(NULL) %>% 
    rename(text = `tweet`) %>% 
    select(text) %>% 
    mutate(document = row_number())
  
  Encoding(colo_tweets$text)  <- "UTF-8"
  
  ToExcludeDend <- c("will","amp", "lol", "#boulder", "im", "dont", 
                     "amp", "@noblebrett", "rt", "ill", "@stapletondenver",
                     "lol", "@dailycamera", "colorado", "ive", "youre")
  
  colo_tokens <- colo_tweets %>%
    unnest_tokens(word, text, "tweets") %>% ## It seems better to use the specific argument to unnest tokens in tweets
    filter(!str_detect(word, "[0-9]"), !str_detect(word, "#"), !word %in% ToExcludeDend)%>% 
    # filter(!str_detect(word, "[0-9]"), !str_detect(word, "#"))%>% 
    anti_join(stop_words)
  
  # colo_dtm <- colo_tokens %>%
  #   count(document, word) %>%
  #   cast_tdm(word, document, n) # Check but I think dtm or tdm depends on the parameters' order.
  
  colo_dtm <- colo_tokens %>% ## CREO QUE ESTE ES EL QUE SIRVE
    count(document, word) %>%
    cast_dtm(document, word, n) # Check but I think dtm or tdm depends on the parameters' order.
  
  colo_dtm <- colo_dtm %>%  # Not sure if now I need this
    removeSparseTerms(0.9975)
  
  colo_matrix <- as.matrix(colo_dtm) #Defining TermDocumentMatrix as matrix
  colo_matrix <- colo_matrix[complete.cases(colo_matrix), ] #Not sure about this
  
  d <- dist(colo_matrix, method="cosine")
  
  return(d) 
}

dists_Denver <- lapply(stages, cosine_dist, clusterID = "1")

plot_silhouette <- function(distancia){
  
  # Using Silhouette coefficient to compute optimum number of clusters
  # See example here: http://www.dcc.fc.up.pt/~ltorgo/DM1_1718/Rclustering.html
  
  methds <- c('ward.D', 'ward.D2')
  avgS <- matrix(NA,ncol=2,nrow=20,
                 dimnames=list(2:21,methds))
  
  for(k in 2:21) 
    for(m in seq_along(methds)) {
      h <- hclust(distancia, meth=methds[m])
      c <- cutree(h,k)
      s <- silhouette(c, distancia)
      avgS[k-1,m] <- mean(s[,3])
    }
  
  dt <- melt(avgS) # formatted as dataframe in long format
  colnames(dt) <- c("NClusts","Meth","AvgS")
  
  ggplot(dt,aes(x=NClusts,y=AvgS,color=Meth)) + 
    geom_line()
}


silhouette_Denver <- lapply(dists_Denver, plot_silhouette)

silhouette_Denver


# Name objects in the list and look for a way to return two things with lapply
# d and the ggplots



hc <- hclust(d, method="ward.D") #It seems that this works better

clustering <- cutree(hc, 2)

p_words <- colSums(colo_matrix) / sum(colo_matrix)

cluster_words <- lapply(unique(clustering), function(x){
  rows <- colo_matrix[ clustering == x , ]
  
  # for memory's sake, drop all words that don't appear in the cluster
  rows <- rows[ , colSums(rows) > 0 ]
  
  colSums(rows) / sum(rows) - p_words[ colnames(rows) ]
})

cluster_summary <- data.frame(cluster = unique(clustering),
                              size = as.numeric(table(clustering)),
                              top_words = sapply(cluster_words, function(d){
                                paste(
                                  names(d)[ order(d, decreasing = TRUE) ][ 1:5 ], 
                                  collapse = ", ")
                              }),
                              stringsAsFactors = FALSE)
cluster_summary




