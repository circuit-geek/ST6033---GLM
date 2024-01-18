rm(list=ls())
attach(mtcars)
mtcars
### using the columns mpg(Y), cyl(X1), wt(X2) for fitting the linear model ###

model1 <- lm(mpg ~ as.factor(cyl) + wt)
summary(model1)

### Interpreting the parameters for cyl ###
# For 6 cylinders cars is lower than 4 cylinders by -4.2556
# For 8 cylinders cars is lower than 4 cylinders by -6.0709

### Interpreting the parameter for wt ###
# For unit change in wt there is decrease in mpg by a factor of 3.2056

### Model Matrix ###
model.matrix(model1)

### Building second model without considering cyl as categorical

model2 <- lm(mpg ~ cyl + wt)
summary(model2)

### Interpreting the parameter for cyl ###
# For unit change in cyl there is decrease in mpg by a factor of 1.5078

### Building third model by adding an interaction term to the second model

model3 <- lm(mpg ~ cyl + wt + cyl*wt)
summary(model3)

### Interpreting the parameters for model 3

# from the p value of interaction term we can say that it is statistically
# significant, which mean it is not appropriate to use the cyl or wt alone
# their estimates and also the interaction effect must be taken into account.
# The effect of wt changes with the number of cyl on the mpg(response variable).

model.matrix(model3) ## product of cyl and wt.
