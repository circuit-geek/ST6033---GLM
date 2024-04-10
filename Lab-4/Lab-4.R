rm(list=ls())

risk = c("L", "M", "H")
yes = c(20, 15, 15)
total = c(200, 100, 50)

log_mod = glm(cbind(yes, total-yes) ~ as.factor(risk), family = 'binomial')
summary(log_mod)

## Calculating probs

p1 = 20/200 ## => 0.1
p2 = 15/100 ## => 0.15
p3 = 15/50 ## => 0.3
p4 = 50/350 ## => 0.142

## Calculating the deviances

## For saturated model deviance => 0

## for reduced model deviance => 11.42
reduced_mod = glm(cbind(yes, total-yes) ~ 1, family = 'binomial')
summary(reduced_mod)

anova(log_mod, reduced_mod)

pchisq(deviance(reduced_mod), df.residual(reduced_mod), lower=FALSE)
## Since the p-value is less than 0.05 we reject H0, both models
## are different.