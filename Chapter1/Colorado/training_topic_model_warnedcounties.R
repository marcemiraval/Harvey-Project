
library(stm) #fast compared to other implementations of topic models. Base don C++
library(furrr)



tweet_in_county_sparse <- readRDS(file = "Chapter1/Colorado/Data/flood_warnedcounties_tweet_sparse.rds")

plan(multicore)

many_models <- data_frame(K = c(seq(3, 20, by = 1))) %>%
  mutate(topic_model = future_map(K, ~stm(tweet_in_county_sparse, K = ., #colo_sparse
                                          verbose = FALSE)))

heldout <- make.heldout(tweet_in_county_sparse)

k_result_county <- many_models %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, tweet_in_county_sparse),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, tweet_in_county_sparse),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

saveRDS(k_result_county, file = "Chapter1/Colorado/Data/k_result_warned_counties.rds")
