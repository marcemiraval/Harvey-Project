

library(twitteR) 
library(ROAuth)

# Twitter authentication
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "Zx6QiuJQXTvrpkbHHPntox5Xs"
consumerSecret <- "TnE9bKf7FeEla5X1rqjkHgOcWjBqL0lCoGNJeWKEYa3KsEPpE5"

setup_twitter_oauth(consumerKey, consumerSecret, 
                    access_token= "274784775-9C4tOo68qtlGRJ56p7gJOQVj2kR3UhRfPHMSrpSS", 
                    access_secret= "9pXKEqQyY4MFbHtBhykB16SxYJBq0ENcXnPYWfJd4tDTO")

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

ids <- scan("HurricaneHarvey_ids.txt", what="", sep="\n")

tweets <- lookupStatus(ids, retryOnRateLimit=100)

tweets_df <- twListToDF(tweets)


