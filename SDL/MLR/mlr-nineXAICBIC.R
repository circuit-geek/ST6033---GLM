rm(list=ls())
setwd("D:/UCC/Academic Work/Sem-2/ST6033 - GLM/SDL/MLR")
### Reads data from the txt file and organizes it into 8406 * 10
dat = matrix(scan(file = "nineXvars.txt"), ncol = 10, byrow = TRUE)
df = as.data.frame(dat)
### Makes the first column as target variable
names(df) = c("y", paste("X", 1:9, sep = ""))
dim(df)
names(df)
pairs(df)

### Fitting a large model with quadratic and interaction terms
fmla = as.formula( paste("y ~ .^2 +", paste("I(X",1:9,"^2)", 
                            sep="", collapse=" + ") ))
mod <- lm( fmla , data = df)
summary(mod)

### Using step function to search the model space for with best AIC
### AIC here is used as the reject criteria.

mod.aic = step(mod, direction = "backward", k = 2)
summary(mod.aic)

exp_mod <- lm(y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + I(X1^2) + I(X2^2) + 
                X1:X2 + X3:X4 + X3:X5 + X3:X8, data = df)
AIC(exp_mod) ### AIC = 18053.14, Adj R.squared = 0.4996
AIC(mod) ### AIC = 18112.38, Adj R.squared = 0.4984
summary(exp_mod)

### Looking at BIC model selection and comparing with AIC

mod.bic = step(mod, direction = "backward", k = log(nrow(df)))
summary(mod.bic)
exp_mod_bic <- lm(y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + I(X1^2) + I(X2^2) + 
                    X1:X2, data = df) ## Adj R.squared = 0.4993
BIC(exp_mod_bic) ## 18154.13
BIC(exp_mod) ## 18172.76
BIC(mod) ## 18506.44
AIC(exp_mod_bic) ## 18055.61
AIC(exp_mod) ## 18053.14
AIC(mod) ## 18112.38

### From the above methods AIC vs BIC, BIC is yielding a model
### with fewer variables, on inspecting the AIC and BIC values
### for both the outcomes, BIC generated model is performing
### slightly better but the R squared value isn't showing
### drastic difference in either cases.

plot(fitted(mod.bic), residuals(mod.bic), pch = ".")
