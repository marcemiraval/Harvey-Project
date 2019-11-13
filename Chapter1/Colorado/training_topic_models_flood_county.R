library(stm) #fast compared to other implementations of topic models. Base don C++
library(furrr)



flood_county_tweet_sparse <- readRDS(file = "Chapter1/Colorado/flood_county_tweet_sparse.rds")



plan(multiprocess)

many_models <- data_frame(K = c(seq(3,24, by = 1))) %>%
  mutate(topic_model = future_map(K, ~stm(flood_county_tweet_sparse, K = ., #colo_sparse
                                          verbose = FALSE)))

heldout <- make.heldout(flood_county_tweet_sparse)

k_result_flood_county <- many_models %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, flood_county_tweet_sparse),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, flood_county_tweet_sparse),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

saveRDS(k_result_flood_county, file = "Chapter1/Colorado/k_result_flood_county.rds")

