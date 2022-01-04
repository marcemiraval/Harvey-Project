
library(stm) #fast compared to other implementations of topic models. Base don C++
library(furrr)



# flood_clusters_tweet_sparse <- readRDS(file = "clusters_flood_sparse.rds") #it didn't work with sparce just with dfm

flood_clusters_tweet_dfm <- readRDS(file = "clusters_flood_dfm.rds")

plan(multiprocess)

many_models <- data_frame(K = c(seq(3,24, by = 1))) %>%
  mutate(topic_model = future_map(K, ~stm(flood_clusters_tweet_dfm, K = ., #colo_sparse
                                          verbose = FALSE)))


heldout <- make.heldout(flood_clusters_tweet_dfm)

k_result_flood_clusters <- many_models %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, flood_clusters_tweet_dfm),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, flood_clusters_tweet_dfm),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

saveRDS(k_result_flood_clusters, file = "k_result_flood_clusters.rds")
