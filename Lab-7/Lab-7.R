rm(list=ls())
Severe <- c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2)
Info <- c(1,1,1,1,2,2,2,2,1,1,1,1,2,2,2,2)
Age <- c(1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4)
n <- c(18,23,22,17,19,35,30,22,24,37,29,24,28,42,37,30)
CB <- c(11,14,11,5,4,15,8,8,10,13,8,6,11,14,15,9)
df = data.frame(Severe, Info, Age, n, CB)

nrow(df) ## Sample size => 16

log_model = glm(cbind(CB, n-CB) ~ as.factor(Severe) + as.factor(Info)
                + as.factor(Age), data = df, family = 'binomial')
summary(log_model)

## Residual deviance => 13.07

leverage = hatvalues(log_model)
leverage[1] ## => 0.3222898

cooks_dist = cooks.distance(log_model)
cooks_dist[1] ## => 0.1497751

pear_res = residuals(log_model, 'pearson')/sqrt(1-leverage)
pear_res[1] ## => 1.37456

dev_res = residuals(log_model)/sqrt(1-leverage)
dev_res[1] ## => 1.377456

plot(pear_res, dev_res) ## both are similar

plot(dev_res)

plot(pear_res)

pear_res[which(pear_res < -2)] ## 1 outlier

dev_res[which(dev_res < -2)] ## 1 outlier

lev_thresh = (2*6)/16 ## 2p/n (0.75)

plot(leverage, ylim = c(0.2, 0.8))
abline(h=0.75) ## 0 points above leverage

plot(cooks_dist)

which.max(cooks_dist) ## 15
