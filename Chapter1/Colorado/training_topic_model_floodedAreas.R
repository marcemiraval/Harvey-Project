
library(stm) #fast compared to other implementations of topic models. Base don C++
library(furrr)



tweetInFlood_sparse <- readRDS(file = "Data/tweetInFlood_sparse.rds")



plan(multiprocess)

many_models <- data_frame(K = c(seq(2, 20, by = 1))) %>%
  mutate(topic_model = future_map(K, ~stm(tweetInFlood_sparse, K = ., #colo_sparse
                                          verbose = FALSE)))

heldout <- make.heldout(tweetInFlood_sparse)

k_result_tweetInFlood <- many_models %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, tweetInFlood_sparse),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, tweetInFlood_sparse),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

saveRDS(k_result_tweetInFlood, file = "Data/k_result_tweetInFlood.rds")
