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
colo_tweets_sf$flood_stage[colo_tweets_sf$date >= min_datetime &
                             colo_tweets_sf$date < "2013-09-09 00:00:00 PDT"] = "Pre_flood"
colo_tweets_sf$flood_stage[colo_tweets_sf$date >= "2013-09-09 00:00:00 PDT" &
                             colo_tweets_sf$date < "2013-09-16 00:00:00 PDT"] = "Flood"
# And the Inmmediate aftermath and post_flood are the same

# My stages
# colo_tweets_sf$flood_stage[colo_tweets_sf$date >= min_datetime & 
#                              colo_tweets_sf$date < "2013-09-11 12:00:00 PDT"] = "Pre_flood"
# colo_tweets_sf$flood_stage[colo_tweets_sf$date >= "2013-09-11 12:00:00 PDT" & 
#                              colo_tweets_sf$date < "2013-09-12 12:00:00 PDT"] = "Flood"
colo_tweets_sf$flood_stage[colo_tweets_sf$date >= "2013-09-16 00:00:00 PDT" &
                             colo_tweets_sf$date < "2013-09-19 00:00:00 PDT"] = "Immediate_Aftermath"
colo_tweets_sf$flood_stage[colo_tweets_sf$date >= "2013-09-19 00:00:00 PDT" &
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
                                     levels(colo_clusters$flood_stage) [c(4,1,2,3)])#reorder factor levels for legend


# creating frequency histograms of reports colored by stage (using create_histo function)
create_histo(InputFile = colo_clusters, HistoColor = NA, HistoBinWidth = 3600,
             HistoName = "gen_hist_1h", SavePath = "Outputs")


####################### CORPUSES PREPARATION AND WORDCLOUDS #################################################

# The list of valid options
stages <<- list("Pre_flood", "Flood", "Immediate_Aftermath", "Post_Flood") # To use when I create function

# Using my corpus function
ToExclude <- c("boulderflood", "boulder", "mdt", "#colorado", "#coflood", "like",
               "will","septemb", "amp", "lol", "#boulder", "im", "#coloradostrong", 
               "#cofloodrelief", "@noblebrett", "rt", "#cowx", "ill", "@stapletondenver",
               "september", "@dailycamera", "colorado", "@boulderflood", "#boulderflood", 
               "youre", "flood", "flooding", "floods", "flooded", "colo", "cu")


# From here on we will be tidytexting

create_wordcloud <- function(stage){
  
  #names(stage)
  #png(filename = names(stage), width=3, height=3, units="in", res=300, bg = "transparent") 
  
  colo_tweets <- colo_clusters %>%
    filter(cluster == "1" | cluster == "2" | flood_stage == stage) %>% 
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

colo_tweets <- colo_clusters %>%
  filter(cluster == "1" | cluster == "2") %>% 
  st_set_geometry(NULL) %>% 
  select(tweet) %>% 
  rename(text = `tweet`) %>% 
  iconv(to = "utf-8", sub="") %>% 
  tolower() %>%
  {gsub("@*", "", .)} %>% # remove at
  {gsub("#\\w+", "", .)} %>% 
  {gsub("http[[:alnum:][:punct:]]*", "", .)} # remove links http


colo_corpus <- Corpus(VectorSource(colo_tweets)) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(stripWhitespace) %>% 
  tm_map(stemDocument) %>% 
  tm_map(removeWords, stopwords("english"))

colo_dtm <- DocumentTermMatrix(colo_corpus)
colo_dtm <- removeSparseTerms(colo_dtm, 0.999999)
colo_matrix <- as.matrix(colo_dtm) #Defining TermDocumentMatrix as matrix
cos_dist <- cosine(colo_matrix) # calculate cosine metric
cos_dist <- as.dist(1- cos_dist) # convert to dissimilarity distances
hc <-hclust(cos_dist)

dendro <- as.dendrogram(hc)
ddata <- dendro_data(dendro, type="rectangle")

ggdendrogram(hc, rotate = FALSE, size = 2)

# prepare dendogram
dendro <- dendro_data(hc, type="rectangle") # convert for ggplot
cutForCluster <- cutree(hc, k= 3) # k is the number of clusters

word_freqs = sort(colSums(colo_matrix), decreasing=TRUE)

clust.df <- data.frame(label=names(cutForCluster), cluster=factor(cutForCluster), 
                       freq=word_freqs)
# dendr[["labels"]] has the labels, merge with clust.df based on label column
dendro[["labels"]] <- merge(dendro[["labels"]], clust.df, by="label")

# plot the dendrogram; note use of color=cluster in geom_text(...)
Dendogram <- ggplot() + 
  geom_segment(data=segment(dendro), aes(x=x, y=y, xend=xend, yend=yend),
               size = 0.4, colour = " Dark gray") +
  geom_text(data=label(dendro), aes(x, y, label=label, hjust=0, color=cluster), 
            size=7) + coord_flip() + scale_y_reverse(expand=c(0.5, 0)) +
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill=NULL),
        panel.grid=element_blank())+
  scale_color_manual(values = brewer.pal(nclusters, palette),
                     guide = 'none')
Dendogram <- Dendogram + theme_void()

Dendogram
ggsave(filename = OutFile, plot = Dendogram, path = OutFolder, width=5, 
       height=10, units="in", dpi = 300, bg = "White")
# colors defined by http://tools.medialab.sciences-po.fr/iwanthue/



######################################
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
