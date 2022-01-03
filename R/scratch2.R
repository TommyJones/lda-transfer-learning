# attempt to calculate a type of ROC curve for topic identification on hdist

library(tidyverse)
library(tidytext)
library(tidylda)


load("data-derived/2021-12-25-23-39-26/hdist.rds")

calc_curve <- function(hdist_mat) {
  
  thresholds <- seq(0, 1, by = 0.01)
  
  out <- apply(hdist_mat, 2, function(x) {
    sapply(thresholds, function(y) sum(x < y))
  })
  
  out
}

curve_matrices <-
  parallel::mclapply(
    X = hdist,
    FUN = function(x) {
      lapply(x$hdist, calc_curve)
    },
    mc.cores = parallel::detectCores() - 1
  )

model_curves <-
  parallel::mclapply(
    X = curve_matrices, 
    FUN = function(x) {
      out <- map(x, function(y) {
        tibble(
          num_matches = rowSums(y)
        ) %>%
          mutate(
            score = log(abs(num_matches - 25) + 1),
            threshold = seq(0, 1, by = 0.01)
            ) 
      }) 
      
      for (j in seq_along(out)) {
        out[[j]]$iteration <- j
      }
      
      out <- bind_rows(out)
      
      out %>%
        select(
          iteration,
          threshold,
          num_matches,
          score
        )
        
    }, mc.cores = parallel::detectCores() - 1)




