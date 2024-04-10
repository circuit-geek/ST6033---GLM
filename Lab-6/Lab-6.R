rm(list = ls())
#install.packages("foreign")
library(foreign) ## Needed to read stata files
setwd("D:/UCC/Academic Work/Sem-2/ST6033 - GLM/Lab/ST6033---GLM/Lab-6")
df = read.dta("mroz.dta")
attach(df)
names(df)

log_mod = glm(inlf ~ age + kidslt6 + kidsge6, data = df, family = 'binomial')
summary(log_mod)

### Calculating the residuals and diagnostic measures ###

leverage = lm.influence(log_mod)
lev_hat_vals = leverage$hat

## Alternative method for leverage values##
#leverage_hat_vals = hatvalues(log_mod)

pear_res = residuals(log_mod, 'pearson')/sqrt(1-lev_hat_vals)
dev_res = residuals(log_mod, 'deviance')/sqrt(1-lev_hat_vals)

## cook_dist = (pear_res*pear_res*h)/(p*(1-h)) (h => leverage)

cooks_dist = (pear_res*pear_res*lev_hat_vals)/(4*(1-lev_hat_vals))

## Alternative method for cooks distance ##
#alt_cooks_dist = cooks.distance(log_mod)

log_linear_preds = log_mod$linear.predictors
plot(log_linear_preds, pear_res)
## ideal plot shouldn't show any trend, should have random scatter
## about zero with constant range, here curved like pattern is visible.
## two bands representing the two classes. [1 and 0]

pchisq(deviance(log_mod), df.residual(log_mod), lower=FALSE)
## p-value = 2.29*10^-7 (Indicating bad fit for the model) ## 

plot(pear_res)
#identify(pear_res, n=2) (? not working)

pear_res[which(pear_res > 2)] ## 3 cases - [74, 79, 400]
pear_res[which(pear_res < -2)] ## 3 cases - [502, 588, 734]
## on considering the usual (+- 2 cutoff)

plot(lev_hat_vals)
text(lev_hat_vals, labels = seq_along(lev_hat_vals), pos = 1)
## leverage_threshold = 2*p/n ##
lev_thresh = (2*4)/753
abline(h=lev_thresh)
length(lev_hat_vals[which(lev_hat_vals>lev_thresh)]) ## 53 cases of high leverage
## 2 cases have very high leverage - [53 and 720]

plot(cooks_dist)
text(cooks_dist, labels = seq_along(cooks_dist), pos = 1)
## 2 cases have high influence - [74 and 400]

## The points that require investigation are 74 and 400 (Potential Outlier and High Influence)

df[74, c(1,3,4,5)]
df[400,c(1,3,4,5)]

df[c(53, 720), c(1,3,4,5)]

## On investigating all 4 points we can conclude that their 
## values are within the permitted limit and no further investigation
## is required.
