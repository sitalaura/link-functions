# datasim
library(ggplot2)
library(effects)
library(glmmTMB)
library(patchwork)

set.seed(3)
N = 250
age = round(runif(N,6,10),1)
group = rbinom(N,1,.5)
y = rpois(N, exp(5.5 - age*.5 + group*.8))

d = data.frame(id=1:length(y), y, age, group=as.factor(group))
fitL_1 = glm(y ~ age * group + (1|id), data=d)
fitP = glm(y ~ age*group, family=poisson(link="log"), data=d)

# per vedere i falsi positivi
niter <- 1000
N <- 250

pvals_log <- rep(NA, niter)

for(i in 1:niter){
  
  age   <- round(runif(N, 6, 10), 1)
  group <- rbinom(N, 1, .5)
  
  # DGP SENZA interazione: solo effetti principali su scala log
  mu <- exp(5.5 - 0.5*age + 0.8*group)
  y  <- rpois(N, lambda = mu)
  
  d <- data.frame(y = y, age = age, group = factor(group))
  
  # Modello "corretto": Poisson con link log, ma includo comunque l'interazione
  fitP <- glm(y ~ age * group, family = poisson(link = "log"), data = d)
  
  # p-value del termine di interazione
  pvals_log[i] <- summary(fitP)$coefficients["age:group1", "Pr(>|z|)"]
}

# Percentuale di falsi positivi (attesa ~5% se tutto è calibrato)
mean(pvals_log < 0.05)


# ---- identity link (modello sbagliato) ----

set.seed(3)

niter <- 1000
N <- 250

pvals_id <- rep(NA, niter)

for(i in 1:niter){
  
  age   <- round(runif(N, 6, 10), 1)
  group <- rbinom(N, 1, .5)
  
  # DGP SENZA interazione: solo effetti principali su scala log (Poisson)
  mu <- exp(5.5 - 0.5*age + 0.8*group)
  y  <- rpois(N, lambda = mu)
  
  d <- data.frame(y = y, age = age, group = factor(group))
  
  # Modello "sbagliato": Gaussian con link identity (equivale a lm)
  fitL <- glm(y ~ age * group, family = gaussian(link = "identity"), data = d)
  
  # p-value dell'interazione (test t)
  pvals_id[i] <- summary(fitL)$coefficients["age:group1", "Pr(>|t|)"]
}

# Percentuale di falsi positivi (qui tipicamente >> 0.05)
mean(pvals_id < 0.05)
