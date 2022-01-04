
library(stm) #fast compared to other implementations of topic models. Base don C++
library(furrr)

clusters_floodAndIA_sparse <- readRDS(file = "Chapter1/Colorado/Data/clusters_floodAndIA_sparse.rds") #it didn't work with sparce just with dfm

plan("multicore")

many_models <- data_frame(K = c(seq(3,24, by = 1))) %>%
  mutate(topic_model = future_map(K, ~stm(clusters_floodAndIA_sparse, K = ., #colo_sparse
                                          verbose = FALSE)))
  

heldout <- make.heldout(clusters_floodAndIA_sparse)

k_result_floodAndIA_clusters <- many_models %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, clusters_floodAndIA_sparse),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, clusters_floodAndIA_sparse),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

saveRDS(k_result_floodAndIA_clusters, file = "Chapter1/Colorado/Data/k_result_floodAndIA_clusters.rds")
