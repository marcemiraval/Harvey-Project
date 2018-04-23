library(ggplot2) # Nice plots.
library(tm) # For text Mining
library(wordcloud)
library(dplyr)
library(stringr)
library(scales) # for date_breaks function
library(lsa) # To compute cosine metric
library(ggdendro)

#to use my package
library(devtools)
library(EWEReporting)



####################### DATA READING AND CLEANING ###############################

# Defining working directory
setwd ("/home/marcela/Coding/EWE-reporting/Colorado")

# Loading data into R
colorado <- read.csv("ColoradoFloodOriginal.csv", header = TRUE, sep = ",")

# Removing duplicated tweets and tweets created by bots
colorado=na.omit(colorado)
colorado$t_text <- str_to_lower(colorado$t_text) # Converting text to lower_case letter
TweetsToExclude <- c("i'm at", "vegas", "#job", "tweetmyjobs", "mountain","game","shirts",
                     "i like that b", "playboy ranks", "it's a rock","bear","hike","football")

# grepl function doesn't take all elements in the vector.
# So we have to paste them with an or stament like "i'm at|vegas"
colorado <- colorado[!grepl(paste(TweetsToExclude, collapse = "|"), colorado$t_text),]
colorado <- distinct(colorado, t_text, .keep_all = TRUE) # remove duplicate tweets
write.csv(colorado, file = "Colorado_Clean.csv")

######################### HISTOGRAMS ###########################################

# Histogram including all reports
colorado$date <-as.POSIXct(strptime(colorado$created_at, format="%Y-%m-%d %H:%M:%S"))

# Creating an attribute to define flood_stage for each report
min_datetime <- "2013-09-01 00:00:48 PDT"
max_datetime <- "2013-09-30 23:52:05 PDT"
colorado$flood_stage = "Pre_flood" # Initialize Variable
# colorado$flood_stage[colorado$date >= min_datetime & colorado$date < "2013-09-09 00:00:00 PDT"] = "Pre_flood"
# colorado$flood_stage[colorado$date >= "2013-09-09 00:00:00 PDT" & colorado$date < "2013-09-16 00:00:00 PDT"] = "Flood"
# colorado$flood_stage[colorado$date >= "2013-09-16 00:00:00 PDT" & colorado$date < "2013-09-23 00:00:00 PDT"] = "Immediate_Aftermath"
# colorado$flood_stage[colorado$date >= "2013-09-23 00:00:00 PDT" & colorado$date  <= max_datetime] = "Post_Flood"
colorado$flood_stage[colorado$date >= min_datetime & colorado$date < "2013-09-11 00:00:00 PDT"] = "Pre_flood"
colorado$flood_stage[colorado$date >= "2013-09-11 00:00:00 PDT" & colorado$date < "2013-09-16 00:00:00 PDT"] = "Flood"
colorado$flood_stage[colorado$date >= "2013-09-16 00:00:00 PDT" & colorado$date < "2013-09-23 00:00:00 PDT"] = "Immediate_Aftermath"
colorado$flood_stage[colorado$date >= "2013-09-23 00:00:00 PDT" & colorado$date  <= max_datetime] = "Post_Flood"

# creating frequency histograms of reports colored by stage (using create_histo function)
OutputsFile <- "/home/marcela/Coding/EWE-reporting/Colorado/Outputs/mystages/"
create_histo(InputFile = colorado, HistoColor = NA, HistoBinWidth = 3600,
             HistoName = "gen_hist_1h", SavePath = OutputsFile)
create_histo(InputFile = colorado, HistoColor = "black", HistoBinWidth = 21600,
             HistoName = "gen_hist_6h", SavePath = OutputsFile)


####################### CORPUSES PREPARATION #################################################

# Filtering out reports sent within specific stages (time intervals)
preFlood <- subset(colorado, (flood_stage == "Pre_flood"))
flood <- subset(colorado, (flood_stage == "Flood"))
iAftermath <- subset(colorado, (flood_stage == "Immediate_Aftermath"))
postFlood <- subset(colorado, (flood_stage == "Post_Flood"))

# Using my corpus function
ToExclude <- c("boulderflood", "flood", "boulder", "mdt", "colorado", "coflood", "like",
               "will","septemb", "just", "amp", "colo", "love", "can", "one", "stay", "get",
               "safe", "see", "look", "morn", "issu", "dont", "lol")

colorado_corpus <- create_corpus(colorado, ToExclude)
preco_corpus <- create_corpus(preFlood, ToExclude)
floodco_corpus <- create_corpus(flood, ToExclude)
iaco_corpus <- create_corpus(iAftermath, ToExclude)
post_co_corpus <- create_corpus(postFlood, ToExclude)


####################### WORDCLOUD AND DENDROGRAM FOR COMPLETE DATASET ###############################

# wordclouds and dendrogram for each stage are in separate .R files

# Constructing the Term Document Matrices and Wordclouds

dtm_colorado <-  DocumentTermMatrix(colorado_corpus)
create_wordcloud(DTMInput = dtm_colorado, sparceFactor = 0.999999999999,
                 OutFolder = OutputsFile,
                 OutFile = "WordcloudAllColorado.png", background = "Black", 
                 ncolors = 6, palette = "Blues")

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
                 palette = "Set2")

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
