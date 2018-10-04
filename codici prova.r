
 twogroup_fun <- function(nrep, b0, b1, sigma) {

  ngroup = 2
  group = rep( c("group1", "group2"), each = nrep)
  eps = rnorm(ngroup*nrep, 0, sigma)
  growth = b0 + b1*(group == "group2") + eps
  growthfit = lm(growth ~ group)
  growthfit
}


library(purrr)
library(broom)
library(ggplot2)
library(dplyr)
sims = rerun(100, twogroup_fun(5,100,0.5,10) ) 

sims %>%
  map_df(tidy) %>% 
  filter(term == "groupgroup2") %>%
  ggplot( aes(estimate) ) +
  geom_density(fill = "blue", alpha = .5) +
  geom_vline( xintercept = 0)
