library(tidyverse)
library(tmsamples)
library(tidytext)
library(tidylda)


load("data-derived/2021-10-18-23-40-23/m-001.rds")
load("data-derived/2021-10-18-23-40-23/pop-pars.rds")


m <- models[[100]]

par <- pars[[1]]$par$par

vocab <- intersect(colnames(m$beta), colnames(par$phi))

ks1 <- apply(par$phi, 1, function(x){
  x <- x[vocab]
  x <- x / sum(x)
  
  k <- ks.test(m$beta[1, vocab], x)
  k$statistic
})

mp <- posterior(m)

t1 <- generate(
  x = mp,
  matrix = "beta",
  which = 1, 
  times = 1000
)

ci <- 
  t1 %>%
  group_by(token) %>%
  summarise(upper = quantile(beta, 0.95), lower = quantile(beta, 0.05))