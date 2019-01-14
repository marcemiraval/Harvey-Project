

library(twitteR) 
library(ROAuth)

# Twitter authentication
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- # Real Value in Generic Functions Folder
consumerSecret <- # Real Value in Generic Functions Folder

setup_twitter_oauth(consumerKey, consumerSecret, 
                    access_token= # Real Value in Generic Functions Folder, 
                    access_secret=)# Real Value in Generic Functions Folder

lookupStatus <- function (ids, ...){
  lapply(ids, twitteR:::check_id)
  
  batches <- split(ids, ceiling(seq_along(ids)/100))
  
  results <- lapply(batches, function(batch) {
    params <- parseIDs(batch)
    statuses <- twitteR:::twInterfaceObj$doAPICall(paste("statuses", "lookup", 
                                                         sep = "/"),
                                                   params = params, ...)
    twitteR:::import_statuses(statuses)
  })
  return(unlist(results))
}

parseIDs <- function(ids){
  id_list <- list()
  if (length(ids) > 0) {
    id_list$id <- paste(ids, collapse = ",")
  }
  return(id_list)
}

ids <- c("898676461032189952", "898679833554759680")
tweets <- lookupStatus(ids, retryOnRateLimit=100)

tweets_df <- twListToDF(tweets)

