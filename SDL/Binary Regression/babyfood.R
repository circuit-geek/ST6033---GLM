rm(list = ls())
setwd("D:/UCC/Academic Work/Sem-2/ST6033 - GLM/Lab/ST6033---GLM/SDL/Binary Regression")
df <- read.table("babyfood.txt")
attach(df)

xtabs(disease/(disease+nondisease) ~ sex + food, df)
### The above gives a contingency table based on the given formula
### so for this case the proportion of diease is given for each gender
### based on their food intake

round(xtabs(disease/ (disease+nondisease) ~ sex + food, df), 3)
### Rounded values for the proportion of disease upto 3 places

mytab = xtabs(disease/(disease+nondisease) ~ sex + food, df)
plot(mytab, main="Mosaic Plot")

options( show.signif.stars = FALSE)
log_model <- glm(cbind(disease, nondisease) ~ sex + food, 
                 family = binomial, data = df)
summary(log_model)

drop1(log_model,test="Chi")

coefficients(log_model)
