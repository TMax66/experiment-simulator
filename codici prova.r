# 
#  twogroup_fun <- function(nrep, b0, b1, sigma) {
# 
#   ngroup = 2
#   group = rep( c("group1", "group2"), each = nrep)
#   eps = rnorm(ngroup*nrep, 0, sigma)
#   growth = b0 + b1*(group == "group2") + eps
#   growthfit = lm(growth ~ group)
#   growthfit
# }


library(purrr)
library(broom)
library(ggplot2)
library(dplyr)
library(DT)
source("2group.r")
sims = rerun(100, twogroup_fun(1000,885,10,60) ) 

sims %>%
  map_df(tidy) %>% 
  filter(term == "groupgroup2") %>%
  ggplot( aes(estimate) ) +
  geom_density(fill = "blue", alpha = .5) +
  geom_vline( xintercept = 10)+
  labs(x="effect")


sims %>%
  map_dbl(~summary(.x)$sigma) %>%
  data.frame(sigma = .) %>%
  ggplot( aes(sigma) ) +
  geom_density(fill = "blue", alpha = .5) +
  geom_vline(xintercept = 60)

sims %>%
  map_dbl(~summary(.x)$sigma) %>%
  {. < 60} %>%
  mean()

sims %>%
  map_df(tidy) %>%
  filter(term == "groupgroup2") %>%
  pull(p.value) %>%
  {. <  0.05} %>%
  mean()



sims = rerun(100, twogroup_fun(1000,885,10,5) ) 
sims %>% 
  map_df(tidy) %>% 
  filter(term == "groupgroup2") %>%
  mutate(simulation=seq(1:n())) %>% 
    ggplot(aes(x = estimate, xmin = estimate-std.error, xmax =estimate+std.error, y=simulation))+
    geom_point() + 
    geom_segment( aes(x = estimate-std.error, xend = estimate+std.error, y=simulation, yend=simulation))+
  #scale_x_continuous(limits=c(estimate-3*std.error, estimate+3*std.error))+
  labs(x="effect")
  
sims %>% 
  map_df(tidy) %>% 
  filter(term == "(Intercept)") %>%
  mutate(simulation=seq(1:n())) %>% 
  ggplot(aes(x = estimate, xmin = estimate-std.error, xmax =estimate+std.error, y=simulation))+
  geom_point() + 
  geom_segment( aes(x = estimate-std.error, xend = estimate+std.error, y=simulation, yend=simulation))+
  labs(x="mean of control group")


sims %>%
  map_df(tidy) %>% 
  filter(term == "groupgroup2") %>% 
  select(estimate,std.error,statistic,"p value"=p.value) %>% 
  mutate_at('p value', round, 5) %>% 
  mutate_at(1:3, round, 3) %>% 
  datatable(options=list())
   

sims %>%
  map_df(tidy) %>%
  filter(term == "groupgroup2") %>%
  ggplot( aes(p.value) ) +
  geom_histogram(color="black",fill = "blue", alpha = .5)+
  geom_vline(xintercept = 0.05)
