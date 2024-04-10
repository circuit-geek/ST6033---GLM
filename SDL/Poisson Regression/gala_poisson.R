rm(list=ls())
library("daewr")
setwd("D:/UCC/Academic Work/Sem-2/ST6033 - GLM/Lab/ST6033---GLM/SDL/Poisson Regression")
df = read.table("gala.txt")
options(show.signif.stars=FALSE)
new_df = df[,-2]
attach(new_df)

poi_model = glm(Species~., data = new_df, family = "poisson")
summary(poi_model)

deviance(poi_model)

## From the p-value obtained (7.078*10^-136) which tells us that
## the evidence doesn't support H0(Good fit for the data), 
## we reject the null hypothesis and accept Ha(Bad fit for the data).
pchisq(deviance(poi_model), df.residual(poi_model), lower = FALSE)

par(mfrow=c(1,2))

## Halfnorm plots helps us to identify if the residuals are
## following the normal trend - Assumption of poisson regression
halfnorm(residuals(poi_model))

## The below plot is a plot between fitted values and squared residuals
## this plot is a tool to assess how good the model is fit, and
## to check if all the assumptions in the model are satisfied.
plot(log(fitted(poi_model)),log((new_df$Species-fitted(poi_model))^2),
     
     xlab=expression(hat(mu)), ylab=expression((y-hat(mu))^2))

abline(0,1)


## dp calculates the overdispersion according to the pearson residual
## summary of the model is adjusted for the new std.err and p value.
(dp = sum(residuals(poi_model,type="pearson")^2)/poi_model$df.res)
summary(poi_model,dispersion=dp)
### (?doubt) - Why are the estimate values changing?

drop1(poi_model, test = "F") ## Quasipoisson (Taken overdispersion into account)

