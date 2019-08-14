
library(stm) #fast compared to other implementations of topic models. Base don C++
library(furrr)


setwd("R:/Chapter1/Colorado")
colo_tidy <- readRDS(file = "colo_tidy.rds")



plan(multiprocess)

many_models <- data_frame(K = c(seq(6, 20, by = 1))) %>%
  mutate(topic_model = future_map(K, ~stm(colo_sparse, K = ., #colo_sparse
                                          verbose = FALSE)))

heldout <- make.heldout(colo_sparse)

k_result <- many_models %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, colo_sparse),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, colo_sparse),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

saveRDS(k_result, file = "k_result.rds")