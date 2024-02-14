rm(list = ls())
setwd("D:/UCC/Academic Work/Sem-2/ST6033 - GLM/Lab/ST6033---GLM/SDL/Binary Regression")
df <- read.table("troutegg.txt")
attach(df)

plot(df) ### Visualizing the data, location and period are Categorical

ftable(xtabs(cbind(survive, total) ~ location + period, df))
### The above provides flat contingency table the values for 
### each location and period the count surive and total.
xtabs(cbind(survive, total) ~ location + period, df)

log_mod <- glm(cbind(survive, total-survive) ~ location + period,
               family = binomial, df) 
options( show.signif.stars = TRUE)
summary(log_mod)
log_mod

#install.packages("daewr")
library(daewr)
halfnorm(residuals(log_mod))
### On assessing the half normal plot max'm points lie close to the
### reference lines, which indicated that the residuals follow normal
### distribution, but residuals points 4, 20 and 9 needs to be investigated.

elogits <- log((survive+0.5)/(total-survive+0.5))
with(df,interaction.plot(period,location,elogits, 
                               main="Interaction Plot"))

sigma2 <- sum(residuals(log_mod,type="pearson")^2)/12

drop1(log_mod,scale=sigma2,test="F")

summary(log_mod,dispersion=sigma2)
summary(log_mod)
