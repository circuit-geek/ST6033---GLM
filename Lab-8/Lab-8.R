rm(list=ls())
setwd("D:/UCC/Academic Work/Sem-2/ST6033 - GLM/Lab/ST6033---GLM/Lab-8")
df = read.table("claims.txt", header = TRUE)
attach(df)

nrow(df) ## samples in the data =>

table(df$Claims) ## 0 highest frequency => 620

barplot(df$Claims) ## left skewed

poi_model = glm(Claims ~ as.factor(Gender) + Age + Mileage + as.factor(Province), data = df, family = 'poisson')
summary(poi_model)
## Residual Deviance => 828.20

pchisq(deviance(poi_model), df.residual(poi_model), lower=FALSE)
## p-value => 0.9401812 (Model is a good fit)

leverage = hatvalues(poi_model) ## Diagonal elements of hat matrix is leverage values
leverage[1] ## => 0.00368

pear_res = residuals(poi_model, 'pearson')/sqrt(1-leverage)
pear_dev = residuals(poi_model)/sqrt(1-leverage)

cooks.distance(poi_model)[1] 
pear_dev[1]

plot(pear_res, pear_dev) ## Both are similar with some values in pearson being extreme

plot(pear_dev)
## ? doubts what do the bands mean?
## Good fit

lp = poi_model$linear.predictors
plot(lp, pear_dev)

length(pear_dev[which(pear_dev > 2)]) ## 18
length(pear_dev[which(pear_dev > 3)]) ## 2
pear_dev[which(pear_dev > 3)] ## 90 and 668

plot(leverage)
lev_thresh = (2*7)/900 ## 2p/n => Leverage value => 0.0156

length(leverage[which(leverage > lev_thresh)]) ## 41 cases 
leverage[which(leverage > lev_thresh)]
which.max(leverage) ## 465

cooks_dist = cooks.distance(poi_model)
plot(cooks_dist)
cooks_dist[which(cooks_dist > 0.02)] ## 90, 367, 395, 668
