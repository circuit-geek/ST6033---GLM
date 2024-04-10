rm(list=ls())
setwd("D:/UCC/Academic Work/Sem-2/ST6033 - GLM/Lab/ST6033---GLM/SDL/Poisson Regression")
df = read.table("dicentric.txt")
attach(df)
round(xtabs(ca/cells ~ doseamt + doserate, df), 2)
par(mfrow=c(1,1))
with(df,interaction.plot(doseamt,doserate,ca/cells))
title(main="Interaction Plot for Rates")

df$dosef = as.factor(df$doseamt)
pmod = glm(ca ~ log(cells) + log(doserate)*dosef, data = df, 
           family = "poisson")
summary(pmod)

## Adding offset while modelling rate data.
rmod <- glm(ca ~ offset(log(cells))+log(doserate)*dosef, family=poisson,df)
summary(rmod)

## p-value = 0.4 (Suggesting good fit)
## but massive difference in Null and Residual deviance
pchisq(deviance(rmod), df.residual(rmod), lower = FALSE)
