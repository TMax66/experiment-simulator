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
    labs(title="Conoscenza a priori", x="Probabilità di Guarigione", y="probability")+
    scale_y_continuous(labels=NULL)+
    geom_area(fill = "lightblue",alpha=0.6)+
    geom_vline(xintercept=0.64,linetype="dashed")
   


df %>% 
    ggplot(aes(theta, likelihood)) + 
    geom_line(col = "cornflowerblue")+
    labs(title="Evidenza sperimentale",x="Probabilità di Guarigione")+
    scale_y_continuous(labels=NULL)+
    geom_area(fill = "lightblue",alpha=0.6)+
    geom_vline(xintercept=22/30,linetype="dashed")



df  %>% 
    ggplot(aes(theta, posterior)) + 
    geom_line(col = "cornflowerblue")+
    labs(title="Probabilità a posteriori",x="Probabilità di Guarigione")+
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

theta1<-dbinom(0:10,10, 0.1)
theta2<-dbinom(0:10,10,0.2)
theta3<-dbinom(01:10,10,0.3)
theta4<-dbinom(01:10,10,0.4)
theta5<-dbinom(01:10,10,0.5)
theta6<-dbinom(01:10,10,0.6)
theta7<-dbinom(01:10,10,0.7)
theta8<-dbinom(01:10,10,0.8)
theta9<-dbinom(01:10,10,0.9)
theta10<-dbinom(01:10,10,1)

L<-cbind(theta1,theta2,theta3,theta4,theta5,theta6,theta7,theta8,theta9,theta10)


L<-tibble(osservazione=c(0:10),theta1=dbinom(0:10,10, 0.1), theta2=dbinom(0:10,10, 0.2),theta3=dbinom(0:10,10,0.3), 
          theta4=dbinom(0:10,10,0.4),theta5=dbinom(0:10,10,0.5),theta6=dbinom(0:10,10,0.6),
          theta7=dbinom(0:10,10,0.7),theta8=dbinom(0:10,10,0.8),theta9=dbinom(0:10,10,0.9),
          theta10=dbinom(0:10,10,1))


L %>% 
  ggplot(aes(x=osservazione, y=theta1))+geom_col()+
  scale_x_continuous(breaks=c(0:10))
                                                 


L0<-data.frame("tetha"=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),"likelihood"=t(L[1,2:10]))
L0 %>% 
  ggplot(aes(x=tetha, y=likelihood))+geom_line()+
  scale_x_continuous(breaks=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))

