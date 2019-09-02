
library(stm) #fast compared to other implementations of topic models. Base don C++
library(furrr)



clusters_sparse <- readRDS(file = "Chapter1/Colorado/clusters_sparse.rds")



plan(multiprocess)

many_models <- data_frame(K = c(seq(2, 30, by = 1))) %>%
  mutate(topic_model = future_map(K, ~stm(clusters_sparse, K = ., #colo_sparse
                                          verbose = FALSE)))

heldout <- make.heldout(clusters_sparse)

k_result_clusters <- many_models %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, clusters_sparse),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, clusters_sparse),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

saveRDS(k_result_clusters, file = "k_result_clusters.rds")

