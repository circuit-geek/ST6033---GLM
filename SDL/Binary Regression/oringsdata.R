rm(list=ls())
setwd("D:/UCC/Academic Work/Sem-2/ST6033 - GLM/Lab/ST6033---GLM/SDL/Binary Regression")
df <- read.table("orings.txt")

### Looking at the data (Visualizing the data) ###
plot(df, xlim = c(40, 90), ylim = c(0, 6), xlab = "Launch Temperature",
     ylab = "O-Ring Damage")

### The highlighted points where damage has occured on the
### o-rings, they are highlighted in red.
points(subset(df, subset = damage > 0), pch=16, col="red")

### If the points along the y-axis are 0 then there is no
### damage occured on the O-rings, they are highlighted in
### blue.
points(subset(df, subset = damage == 0), pch=15, col="blue")

attach(df)
### The cbind is done here to fit the logistic model, the cmd
### makes two columns, giving the counts(instances), where
### there was damage in the first column and in the second 
### column it gives the count of non damaged ones, this matrix
### is used for fitting the logisitic model using glm.
cbind(damage, 6-damage)

### Fitting the model

log_model <- glm(cbind(damage , 6-damage) ~ temp, 
                 family = "binomial" , data = df)
summary(log_model)

### From summary of the log_model we can observe that the
### no of iterations taken to reach the max'm likelihood was 
### 6 cycles have occured.

x <- seq(25, 85, 1)
Xmat <- cbind(rep(1, length(x)), x)
eta <- Xmat %*% log_model$coefficients
lines(x, inv.logit(eta)*6, lwd=3)

#install.packages("LaplacesDemon") ### The package needed to perform
library(LaplacesDemon)            ### inverse logit function.

x <- seq(25, 85, 1)
Xmat <- cbind(rep(1, length(x)), x)
eta <- Xmat %*% log_model$coefficients
lines(x, invlogit(eta)*6, lwd=3)

### In the above first we create a sequence x from 25-85, later we 
### define a matrix which has two columns 1's to the len(x) and x
### To calculate eta (Logit function - Log Odd Values), we multiply
### the matrix with the coeffiecient(Beta.0 and Beta.1) => eta values.
### To fit the sigmoid curve to the data we need to obtain the probs,
### We calculate the inverse logit to gets the probs and fit the curve.

log_model_2 <- glm(cbind(damage, 6-damage) ~ temp, 
                   family = binomial(link = probit), data = df)
summary(log_model_2)

### Fitting using probit model to see changes

Xmat_b = cbind(rep(1, length(x)), x)
eta_b = Xmat %*% log_model_2$coefficients
lines(x, pnorm(eta_b)*6, lwd = 3, col="orange", lty=2)

### Detailed analysis for Logit link function

summary(log_model)
deviance(log_model) ### The deviance of the model is 16.91228
pchisq(deviance(log_model),df.residual(log_model),lower=FALSE)

### From the p-value obtained from the model which is 0.716,
### There is no statistical significance between the associated 
### variables. In this case the H0: There is no relationship between
### the variables. And based on the p-value we fail to reject the H0.

c(-0.2162 - 2*0.0532, -0.2162 + 2*0.0532)
confint(log_model)

### The conf-int values under the assumption that the coefficient are
### normally distributed and using likelihood methods are returning
### almost same values for confidence intervals.
### 2.5% => -0.3326    97.5% => -0.1201