rm(list=ls())
#install.packages("faraway")
library(faraway)

y <- c(320,14,80,36)
particle <- gl(2,1,4,labels=c("no","yes"))
quality <- gl(2,2,labels=c("good","bad"))
wafer <- data.frame(y,particle,quality)
wafer
ov <- xtabs(y ~ quality+particle)

mod1 = glm(y ~ particle + quality, data = wafer, family = 'poisson')
summary(mod1)
drop1(mod1,test="Chi")

## The traditional Chi-Squared test of independence...  

(t(model.matrix(mod1)) %*% y)[,]
(pp <- prop.table( xtabs(y ~ particle)))
(qp <- prop.table( xtabs(y ~ quality)))
(fv <- outer(qp,pp)*450)

2*sum(ov*log(ov/fv)) 
sum( (ov-fv)^2/fv)
prop.test(ov)

## Fisher's "exact test" for 2x2 tables... 

(m <- matrix(y,nrow=2))
modb <- glm(m ~ 1, family=binomial)
deviance(modb)
fisher.test(ov)
(320*36)/(14*80)
