  library(tidyverse)

beta_df <- tibble(
  theta = seq(0, 1, by = 0.01),
  p_theta = dbeta(theta, 4, 2)
)
beta_df %>% ggplot(aes(theta, p_theta)) + geom_line()


bern_beta <- function(a, b, N, z) {
  bern_beta_df <- tibble(
    theta = seq(0, 1, by = 0.01),
    prior = dbeta(theta, a, b), # beta prior
    likelihood = theta ^ z * (1 - theta) ^ (N - z), # bernoulli likelihood
    posterior = dbeta(theta, a + z, b + N - z) # beta posterior
  )
  cat("posterior beta parameters", a + z, b + N -z)
  bern_beta_df %>% 
    gather(type, prob, -theta) %>% 
    mutate(type = factor(type, levels = c("prior", "likelihood", "posterior"))) %>% 
    ggplot(aes(theta, prob)) + 
    geom_line(col = "cornflowerblue") + 
    geom_area(fill = "cornflowerblue") +
    facet_wrap(~type, ncol = 1, scales = "free")
}

bern_beta(100,100,20,17)



df<- tibble(
    theta = seq(0, 1, by = 0.01),
    prior = dbeta(theta, 10, 6), # beta prior
    likelihood = theta ^ 22 * (1 - theta) ^ (30 - 22), # bernoulli likelihood
    posterior = dbeta(theta, 10 + 22, 6 + 30 - 22) # beta posterior
  )

df %>% 
    ggplot(aes(theta, prior)) + 
    geom_line(col = "cornflowerblue")+
    labs(title="Conoscenza a priori", x="Probabilit? di Guarigione", y="probability")+
    scale_y_continuous(labels=NULL)+
    geom_area(fill = "lightblue",alpha=0.6)+
    geom_vline(xintercept=0.64,linetype="dashed")
   


df %>% 
    ggplot(aes(theta, likelihood)) + 
    geom_line(col = "cornflowerblue")+
    labs(title="Evidenza sperimentale",x="Probabilit? di Guarigione")+
    scale_y_continuous(labels=NULL)+
    geom_area(fill = "lightblue",alpha=0.6)+
    geom_vline(xintercept=22/30,linetype="dashed")



df  %>% 
    ggplot(aes(theta, posterior)) + 
    geom_line(col = "cornflowerblue")+
    labs(title="Probabilit? a posteriori",x="Probabilit? di Guarigione")+
    scale_y_continuous(labels=NULL)+
    geom_area(fill = "lightblue",alpha=0.6)+
     geom_vline(xintercept=0.70,linetype="dashed")


df %>% 
  gather(type, prob, -theta) %>% 
  mutate(type = factor(type, levels = c("prior", "likelihood", "posterior"))) %>% 
  ggplot(aes(theta, prob)) + 
  geom_line(col = "cornflowerblue") + 
  geom_area(fill = "cornflowerblue") +
  facet_wrap(~type, ncol = 1, scales = "free")

############################################

# theta1<-dbinom(0:10,10, 0.1)
# theta2<-dbinom(0:10,10,0.2)
# theta3<-dbinom(01:10,10,0.3)
# theta4<-dbinom(01:10,10,0.4)
# theta5<-dbinom(01:10,10,0.5)
# theta6<-dbinom(01:10,10,0.6)
# theta7<-dbinom(01:10,10,0.7)
# theta8<-dbinom(01:10,10,0.8)
# theta9<-dbinom(01:10,10,0.9)
# theta10<-dbinom(01:10,10,1)
# 
# L<-cbind(theta1,theta2,theta3,theta4,theta5,theta6,theta7,theta8,theta9,theta10)

options(scipen=999)
options(digits=2)
L<-tibble(osservazione=c(0:10),"P=0.1"=dbinom(0:10,10, 0.1), "P=0.2"=dbinom(0:10,10, 0.2),"P=0.3"=dbinom(0:10,10,0.3), 
          "P=0.4"=dbinom(0:10,10,0.4),"P=0.5"=dbinom(0:10,10,0.5),"P=0.6"=dbinom(0:10,10,0.6),
          "P=0.7"=dbinom(0:10,10,0.7),"P=0.8"=dbinom(0:10,10,0.8),"P=0.9"=dbinom(0:10,10,0.9))




print(xtable(round(L,2)), include.rownames=FALSE,  size="\\tiny")

dprob<-function(y){
  ggplot(data=L, aes_(x=~osservazione, y=as.name(y)))+geom_col()+
    scale_x_continuous(breaks=c(0:10))+
    labs(y="probability", x="n.soggetti guariti",title=bquote(theta==[y] ))
}

title=as.name(y)

prob<-lapply(names(L[,2:10]), dprob)

p1<-prob[[1]]
p2<-prob[[2]]
p3<-prob[[3]]
p4<-prob[[4]]
p5<-prob[[5]]
p6<-prob[[6]]
p7<-prob[[7]]
p8<-prob[[8]]
p9<-prob[[9]]
library(tidyverse)
library("gridExtra")
library(cowplot)










L0<-data.frame("Prob"=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),"likelihood"=t(L[1,2:10]))
L1<-data.frame("Prob"=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),"likelihood"=t(L[2,2:10]))
L2<-data.frame("Prob"=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),"likelihood"=t(L[3,2:10]))
L3<-data.frame("Prob"=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),"likelihood"=t(L[4,2:10]))
L4<-data.frame("Prob"=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),"likelihood"=t(L[5,2:10]))
L5<-data.frame("Prob"=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),"likelihood"=t(L[6,2:10]))
L6<-data.frame("Prob"=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),"likelihood"=t(L[7,2:10]))
L7<-data.frame("Prob"=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),"likelihood"=t(L[8,2:10]))
L8<-data.frame("Prob"=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),"likelihood"=t(L[9,2:10]))
L9<-data.frame("Prob"=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),"likelihood"=t(L[10,2:10]))
L10<-data.frame("Prob"=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),"likelihood"=t(L[11,2:10]))



l0<-L0 %>% 
  ggplot(aes(x=Prob, y=likelihood))+geom_line()+theme_grey(base_size = 8)+
  scale_x_continuous(breaks=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))+labs(title="N.di positivi=0")
l1<-L1 %>% 
  ggplot(aes(x=Prob, y=likelihood))+geom_line()+theme_grey(base_size = 8)+
  scale_x_continuous(breaks=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))+labs(title="N.di positivi=1")

l2<-L2 %>% 
  ggplot(aes(x=Prob, y=likelihood))+geom_line()+theme_grey(base_size = 8)+
  scale_x_continuous(breaks=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))+labs(title="N.di positivi=2")

l3<-L3 %>% 
  ggplot(aes(x=Prob, y=likelihood))+geom_line()+theme_grey(base_size = 8)+
  scale_x_continuous(breaks=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))+labs(title="N.di positivi=3")

l4<-L4 %>% 
  ggplot(aes(x=Prob, y=likelihood))+geom_line()+theme_grey(base_size = 8)+
  scale_x_continuous(breaks=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))+labs(title="N.di positivi=4")

l5<-L5 %>% 
  ggplot(aes(x=Prob, y=likelihood))+geom_line()+theme_grey(base_size = 8)+
  scale_x_continuous(breaks=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))+labs(title="N.di positivi=5")

l6<-L6 %>% 
  ggplot(aes(x=Prob, y=likelihood))+geom_line()+theme_grey(base_size = 8)+
  scale_x_continuous(breaks=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))+labs(title="N.di positivi=6")

l7<-L7 %>% 
  ggplot(aes(x=Prob, y=likelihood))+geom_line()+theme_grey(base_size = 8)+
  scale_x_continuous(breaks=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))+labs(title="N.di positivi=7")

l8<-L8 %>% 
  ggplot(aes(x=Prob, y=likelihood))+geom_line()+theme_grey(base_size = 8)+
  scale_x_continuous(breaks=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))+labs(title="N.di positivi=8")

l9<-L9 %>% 
  ggplot(aes(x=Prob, y=likelihood))+geom_line()+theme_grey(base_size = 8)+
  scale_x_continuous(breaks=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))+labs(title="N.di positivi=9")

l10<-L10 %>% 
  ggplot(aes(x=Prob, y=likelihood))+geom_line()+theme_grey(base_size = 8)+
  scale_x_continuous(breaks=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))+labs(title="N.di positivi=10")

plot_grid(l0,l1,l2,l3,l4,l5,l6,l7,l8,l9,l10)

##################################################




library(ggplot2)
library(gridExtra)

# Make 3 simple graphics:
g1=ggplot(mtcars, aes(x=qsec)) + geom_density(fill="slateblue")
g2=ggplot(mtcars, aes(x=drat, y=qsec, color=cyl)) + geom_point(size=5) + theme(legend.position="none")
g3=ggplot(mtcars, aes(x=factor(cyl), y=qsec, fill=cyl)) + geom_boxplot() + theme(legend.position="none")
g4=ggplot(mtcars , aes(x=factor(cyl), fill=factor(cyl))) +  geom_bar()

# Show the 4 plots on the same page
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)

# Plots
grid.arrange(g2, arrangeGrob(g3, g4, ncol=2), nrow = 2)
grid.arrange(g1, g2, g3, nrow = 3)
grid.arrange(g2, arrangeGrob(g3, g4, ncol=2), nrow = 1)
grid.arrange(g2, arrangeGrob(g3, g4, nrow=2), nrow = 1)





