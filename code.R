
sims = rerun(1000, twogroup_fun(5,885,200,60) )

sims %>%

  map_df(tidy) %>% 
  filter(term == "groupgroup2") %>%
  ggplot( aes(p.value) ) +
  geom_density(fill = "blue", alpha = .5)

  




+
  geom_vline( xintercept = "effect")


############################Analisi dei dati##################################
library(purrr)
library(broom)
library(ggplot2)
library(dplyr)
library(tidyverse)
source("2group.r")
fit<-twogroup_fun(10,150,20,30)
df<-augment(fit)
  df %>% 
  dplyr::select(y, group) %>% 
  mutate(y=round(y, 3)) %>% 
    do(tidy(t.test(y~group, data=.))) %>% 
    mutate("difference"=estimate2-estimate1) %>% 
    dplyr::select(difference)

###############################################################
library(tidyverse)
mean=input$b0
sd=input$sigma
Y<-seq(-4,4,length=100)*sd + mean
d<-tibble(dens=dnorm(Y, mean, sd), Y)

d %>% 
  ggplot(aes(x=Y, y=dens))+geom_line()



set.seed(1234)
mean=1000
sigma=50
each=10000
dat <- tibble(cond = factor(rep(c("Control","Treatment"), each=each)), 
                  y = c(rnorm(each, mean=mean, sd=sigma),rnorm(each, mean=mean-80, sd=sigma)))
  
dat %>% 
  ggplot(aes(x=y, fill=cond)) + geom_density(alpha=.08, adjust=2.5, )+
  scale_fill_grey()+theme(legend.title = element_blank())

dat<-tibble(y = c(rnorm(each, mean=mean, sd=sd)))
dat %>% 
  ggplot(aes(x=y)) + geom_density(alpha=.08, adjust=2.5)


+ theme_classic()






set.seed(1492)
mean=1000
sigma=20
n=1000
min<-mean-6.5*sigma
max<-mean+6.5*sigma
df <- data.frame(
  x = rnorm(n, mean=mean, sd=sigma)
)
x <- df$x
ggplot(df, aes(x))+ 
  stat_function(fun = dnorm, colour = "red", args = list(mean = mean, sd=sigma))+
  stat_function(fun = dnorm, colour = "blue", args = list(mean = mean+80, sd=sigma))+
  xlim(from=min, to=max)
  
