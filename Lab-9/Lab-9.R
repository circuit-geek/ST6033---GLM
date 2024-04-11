rm(list = ls())
setwd("D:/UCC/Academic Work/Sem-2/ST6033 - GLM/Lab/ST6033---GLM/Lab-9")
count = c(51, 992, 41, 245)
sc = c(1, 1, 2, 2)
hd = c(1, 2, 1, 1)
df = data.frame(count, sc, hd)

poi.model1 = glm(count ~ factor(sc) + factor(hd), data=df, family='poisson')
summary(poi.model1)

pchisq(deviance(poi.model1), df.residual(poi.model1), lower=FALSE)
qchisq(0.95, df.residual(poi.model1))


df2 = read.table("Table_5_1.txt", header = TRUE)
chisq.test(df2$SC, df2$HD)

Count<-c(72,66,86,88,76,92,60,56,63)
A<-c(1,2,3,1,2,3,1,2,3)
B<-c(1,1,1,2,2,2,3,3,3)

df3 = data.frame(Count, A, B)
poi.model2 = glm(Count ~ factor(A) + factor(B), data = df3, family = 'poisson')
summary(poi.model2)
pchisq(deviance(poi.model2), df.residual(poi.model2), lower=FALSE)
qchisq(0.95, df.residual(poi.model2))
