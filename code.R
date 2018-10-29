
sims = rerun(1000, twogroup_fun(5,885,200,60) )

sims %>%

  map_df(tidy) %>% 
  filter(term == "groupgroup2") %>%
  ggplot( aes(p.value) ) +
  geom_density(fill = "blue", alpha = .5)

  




+
  geom_vline( xintercept = "effect")


############################Analisi dei dati##################################à
source("2group.r")
fit<-twogroup_fun(10,150,20,30)
df<-augment(fit)
  df %>% 
  dplyr::select(y, group) %>% 
  mutate(y=round(y, 3)) %>% 
    do(tidy(t.test(y~group, data=.)))


  
  dt_result = dt %>% group_by(species) %>% do(tidy(t.test(N15~region, data=.)))
  


