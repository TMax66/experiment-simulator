
sims = rerun(1000, twogroup_fun(5,885,200,60) )

sims %>%

  map_df(tidy) %>% 
  filter(term == "groupgroup2") %>%
  ggplot( aes(p.value) ) +
  geom_density(fill = "blue", alpha = .5)

  




+
  geom_vline( xintercept = "effect")



