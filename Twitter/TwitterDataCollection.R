library(tidyverse)
library(stringr)
library(rtweet)

twitter_key <- scan('~/github/api_keys/twitter.txt', what = 'character')

consumer_key    <- "gDOWYfpjQc4sYsYY0jW5Etp6M"
consumer_secret <- "xNSm15c269lpFzj5WsgFlaoTthWGxIJQNzYusNOwmdHSWiSbSK"

rtweet::create_token(app = "rtweet_sentiments_testing", consumer_key, consumer_secret,
                     set_renv = TRUE)

tornado <- search_tweets("#tornado", n = 18000, include_rts = FALSE)
save(tornado, file="tornado.Rdata")
