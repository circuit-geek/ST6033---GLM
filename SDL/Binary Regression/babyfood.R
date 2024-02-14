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
### The coefficients we have here is log odds, it's easier to interpret
### odds over log odds, for the coefficient. Similarly, the way to 
### to interpret this coefficient for sexGirl => -0.3126 is that there
### is a decrease in the log odds when moving from Boy to Girl, since
### the baseline here is boy.

confint(log_model)
### Confidence interval for the coefficients in the log odds scale
exp(confint(log_model))
### Confidence interval for the coefficients in the odds scale

### under the assumption that the coefficients are normally distributed
### we can use the std.error based calculation after converting them to
### log odds.

exp(c(-0.3126 - 2*0.1410, -0.3126 + 2*0.1410)) ### sexGirl
exp(c(-0.6693 - 2*0.1530, -0.6693 + 2*0.1530)) ### foodBreast
exp(c(-0.1725 - 2*0.2056, -0.1725 + 2*0.2056)) ### foodSuppl

### The confidence interval values are pretty much again equal in both
### cases.
