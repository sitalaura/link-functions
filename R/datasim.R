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
fitP = glm(y ~ age*group, family=poisson(link="log"), data=d)

effP = data.frame(allEffects(fitP, xlevels=list(age=seq(min(age), max(age), .05)))$"age:group")
effP$group = as.factor(effP$group)

ts <- 16 # dimensione testo numerica
ggplot(d, aes(x=age, y=y, shape=group, color=group)) +
  geom_point(size=4, alpha=.6) +
  scale_color_manual(values=c("darkorange2","darkgreen")) +
  scale_fill_manual(values=c("darkorange1","darkgreen")) +
  geom_line(data=effP, aes(x=age, y=fit, group=group, linetype=group), linewidth=2) +
  geom_ribbon(data=effP, aes(x=age, y=fit, ymin=lower, ymax=upper, group=group, fill=group),
              alpha=.3, color=NA) +
  theme(text = element_text(size=ts, color="black")) +
  scale_x_continuous(breaks=seq(0, 20, .5)) +
  ylab("Errors") + xlab("Age (years)")

# datasim con interazione
set.seed(3)
N = 250; age = round(runif(N,6,10),1); group = rbinom(N,1,.5); y = rpois(N,exp(5.5-age*.5+group*.8))
d = data.frame(id=1:length(y),y,age,group=as.factor(group))
fitP = glm(y~age*group, family=poisson(link="log"), data=d)
effP = data.frame(allEffects(fitP,xlevels=list(age=seq(min(age),max(age),.05)))$"age:group")
effP$group = as.factor(effP$group)
ggplot(d,aes(x=age,y=y,shape=group,color=group))+
  geom_point(size=4,alpha=.6)+
  scale_color_manual(values=c("darkorange2","darkgreen"))+
  scale_fill_manual(values=c("darkorange1","darkgreen"))+
  geom_line(data=effP,aes(x=age,y=fit,group=group,linetype=group),size=2)+
  geom_ribbon(data=effP,aes(x=age,y=fit,ymin=lower,ymax=upper,group=group,fill=group),alpha=.3,color=NA)+
  theme(text=element_text(size=ts,color="black"))+scale_x_continuous(breaks=seq(0,20,.5))+
  ylab("Errors")+xlab("Age (years)")