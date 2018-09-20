library(ggplot2) # Nice plots.
library(lubridate)
library(tm) # For text Mining
library(wordcloud)
library(stringr) # for str_to_lower function
library(scales) # for date_breaks function
library(lsa) # To compute cosine metric
library(ggdendro)
library(dplyr)

#to use my package
# library(devtools)
# install("/home/marcela/Coding/EWEReporting")
library(EWEReporting)



####################### DATA READING AND CLEANING ###############################

# Defining working directory
setwd ("Chapter1/Sandy")

# Loading data into R
sandy <- read.csv("Shelton_et_al_Sandy.csv", header = TRUE, sep = ",")

# Removing duplicated tweets and tweets created by bots
sandy=na.omit(sandy)
sandy$text <- str_to_lower(sandy$text)# Converting text to lower_case letter
TweetsToExcludeSandy <- c("#sandy bgz", "#satstudytime") 
# grepl function doesn't take all elements in the vector.
# So we have to paste them with an or stament like "i'm at|vegas"
sandy <- sandy[!grepl(paste(TweetsToExcludeSandy, collapse = "|"), sandy$text),] 
sandy <- distinct(sandy, text, .keep_all = TRUE) # remove duplicate tweets
write.csv(sandy, file = "Sandy_Clean.csv")

######################### HISTOGRAMS ###########################################

# Histogram including all reports
sandy$created_at <- ymd_hms(sandy$created_at, tz ="UTC")

# Creating an attribute to define event_stage for each report 
min_datetime <- min(sandy$created_at)
max_datetime <- max(sandy$created_at)

sandy$event_stage = "Calm" # Initialize Variable
sandy$event_stage[sandy$created_at >= min_datetime & sandy$created_at <
                    "2012-10-28 02:00:00 UTC"] = "PreCalm"
sandy$event_stage[sandy$created_at >= "2012-10-28 02:00:00 UTC" & sandy$created_at <
                    "2012-10-29 02:00:00 UTC"] = "PrePeak"
sandy$event_stage[sandy$created_at >= "2012-10-29 02:00:00 UTC" & sandy$created_at <
                    "2012-10-30 02:00:00 UTC"] = "Peak"
sandy$event_stage[sandy$created_at >= "2012-10-30 02:00:00 UTC" & sandy$created_at <
                    "2012-10-31 02:00:00 UTC"] = "PostPeak"
sandy$event_stage[sandy$created_at >= "2012-10-31 02:00:00 UTC" & sandy$created_at <
                    max_datetime] = "PostCalm"

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
