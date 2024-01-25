rm(list=ls())
attach(LifeCycleSavings)
### Plotting the data ###
pairs(LifeCycleSavings, panel = panel.smooth, 
      main = "LifeCycleSavings data")
### What is saturated model? ###
fm1 <- lm(sr ~ pop15 + pop75 + dpi + ddpi, 
          data = LifeCycleSavings)
summary(fm1)

### Performing the matrix calculation for coefficients ###
x <- model.matrix( ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings)
y <- LifeCycleSavings$sr 
xtxi <- solve(t(x) %*% x)
xtxi %*% t(x) %*% y
solve(crossprod(x,x),crossprod(x,y))

### Regression summary ###
names(fm1)
fm1s <- summary(fm1)
names(fm1s)
sqrt(deviance(fm1)/df.residual(fm1))
fm1s$sigma
xtxi <- fm1s$cov.unscaled
sqrt(diag(xtxi)) * fm1s$sigma
fm1s$coef[,2]
1-deviance(fm1)/sum((y-mean(y))^2)
fm1s$r.squared
### The values above are perfectly matching with the fit ###

### Fitting saturated model again ###

fm1 <- lm(sr ~ pop15 + pop75 + dpi + ddpi, 
          data = LifeCycleSavings)
summary(fm1) ### Adj R.squared = 0.2797
drop1(fm1) ### It drops variable in the model one by one 
           ### and assess the performance
step(fm1) ### This uses AIC to suggest the best model ie: fm2

fm2 <- lm(formula = sr ~ pop15 + pop75 + ddpi, 
          data = LifeCycleSavings)
summary(fm2) 
### Plotting the diagnostic plots ###
par(mfrow=c(2,2))
plot(fm2)

### From the diagnostic plots we can understand that
### the model is not showing any heteroskedascity and 
### from the QQ-Plot the overall model seems a good fit,
### but Libya has a very high leverage, which necessarily 
### need not be a outlier, but has a significant impact on the model
### cook's distance gives a threshold in which above that value
### means significant impact on the model if that point is removed.
