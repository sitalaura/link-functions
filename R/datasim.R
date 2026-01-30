# libraries
library(ggplot2)
library(effects)
library(psyphy)

# simulation w/ 1 independent variable

set.seed(0)

k = 60
N = 400
age = runif(N,6,10)
eta = -5.5+age*0.9
probs = mafc.probit(.m = 2)$linkinv(eta)
accuracy = rbinom(n = N, size = k, prob = probs) / k
d = data.frame(age,accuracy)

# linear model 

fit = lm(accuracy ~ age, data=d)
eff = data.frame(allEffects(fit,xlevels=list(age=seq(6,10,.1)))$"age")
eff$accuracy = eff$fit
ggplot(d,aes(x=age,y=accuracy))+
  geom_point()+
  geom_line(data=eff,color="red",size=2)

d$pp_sim = simulate(fit)$sim_1
ggplot(d,aes(x=age,y=accuracy))+
  geom_point(aes(x=age,y=pp_sim),color="purple",size=3.5,alpha=.4)+
  geom_point()+
  geom_line(data=eff,color="red",size=2)

ggplot(d,aes(x=accuracy))+
  geom_histogram(aes(y=after_stat(density)),color=NA,fill="black",alpha=.4)+
  geom_histogram(aes(x=pp_sim,y=after_stat(density)),color=NA,fill="purple",alpha=.4)


# logistic regression model

fit = glm(accuracy ~ age, data=d, family=binomial(link="logit"),
          weights= rep(k, nrow(d)))
eff = data.frame(allEffects(fit,xlevels=list(age=seq(6,10,.1)))$"age")
eff$accuracy = eff$fit
ggplot(d,aes(x=age,y=accuracy))+
  geom_point()+
  geom_line(data=eff,color="red",size=2)

d$pp_sim = simulate(fit)$sim_1
ggplot(d,aes(x=age,y=accuracy))+
  geom_point(aes(x=age,y=pp_sim),color="purple",size=3.5,alpha=.4)+
  geom_point()+
  geom_line(data=eff,color="red",size=2)

ggplot(d,aes(x=accuracy))+
  geom_histogram(aes(y=after_stat(density)),color=NA,fill="black",alpha=.4)+
  geom_histogram(aes(x=pp_sim,y=after_stat(density)),color=NA,fill="purple",alpha=.4)


###################################

# simulation w/ 2 independent variables
set.seed(0) 

k = 50
N = 1000
group = rbinom(N,1,.5)
age = runif(N,6,10)
eta = -6+1*age-1*group # simulated linear predictor
probs = mafc.probit(.m = 2)$linkinv(eta)
accuracy = rbinom(n = N, size = k, prob = probs) / k

d = data.frame(
  age = age,
  age_c = age - mean(age),
  accuracy = accuracy,
  group = as.factor(group)
)

ggplot(d, aes(x = age, y = accuracy, color = group)) +
  geom_point() +
  scale_x_continuous(limits = c(6, 10), breaks = seq(6, 10, 1))

# identity link
fit = glm(accuracy ~ age*group, data=d)
summary(fit)

# logit
fit = glm(accuracy ~ age*group, data=d, family=binomial(link="logit"),
          weights= rep(k, nrow(d)))
summary(fit)

# probit
fit = glm(accuracy ~ age*group, data=d, family=binomial(link="probit"),
          weights= rep(k, nrow(d)))
summary(fit)

# multiple alternative forced choice (50%) probit
fit = glm(accuracy ~ age*group, data=d, family=binomial(link=mafc.probit(.m=2)),
          weights= rep(k, nrow(d)))
summary(fit)

