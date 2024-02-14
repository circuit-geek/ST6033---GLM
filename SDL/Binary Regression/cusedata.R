rm(list = ls())
setwd("D:/UCC/Academic Work/Sem-2/ST6033 - GLM/Lab/ST6033---GLM/SDL/Binary Regression")
df <- read.table("cuse.dat", header = TRUE)
attach(df)

plot(df)

log_model <- glm(cbind(using, notUsing) ~ age + education + wantsMore,
                 family = binomial, data = df)
summary(log_model)

drop1(log_model)
