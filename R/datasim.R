# datasim
library(ggplot2)
library(effects)
library(glmmTMB)
library(patchwork)
library(psyphy)

set.seed(2)

k = 50
N = 200
age = runif(N,6,10)
accuracy = rbinom(n=N, size=k, prob=plogis(-4.5+age*0.9))/k
plot(age,accuracy)

###################################

set.seed(0)

# un solo gruppo
k = 60
N = 400
age = runif(N,6,10)
eta = -5.5+age*0.9
probs = mafc.probit(.m = 2)$linkinv(eta)
accuracy = rbinom(n = N, size = k, prob = probs) / k
d = data.frame(age,accuracy)
ggplot(d,aes(x=age,y=accuracy))+
  geom_point()

# aggiungiamo secondo gruppo
k = 50
N = 1000
group = rbinom(N,1,.5)
age = runif(N,6,10)
eta = -6+1*age-1*group
probs = mafc.probit(.m = 2)$linkinv(eta)
accuracy = rbinom(n = N, size = k, prob = probs) / k
d = data.frame(age=age-mean(age),accuracy,group=as.factor(group))
ggplot(d,aes(x=age,y=accuracy,color=group))+
  geom_point()+
  xlab("Age-centered")


# identity
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

