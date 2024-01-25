rm(list=ls())
setwd("D:/UCC/Academic Work/Sem-2/ST6033 - GLM/SDL/SLR")
df <- read.table("mens200m.txt", header = TRUE)
attach(df)

model1 <- lm(Men200m ~ Year)
summary(model1)
plot( x = Year , y = Men200m , 
      xlab="Year", ylab="Men's 200m Winning Time",
      main="Are Sprinters Getting Faster?",
      panel.last = lines(sort(Year), fitted(model1)[order(Year)]))

### values from the lm model - (Beta = -0.028383, Intercept = 76.153369)

#### Computing the formula to check if answer coincides with lm

x <- Men200m
y <- Year
xbar <- mean(x)
ybar <- mean(y)
SSxx <- sum( (x-xbar)^2 )
SSyy <- sum( (y-ybar)^2 )
SSxy <- sum( (x-xbar)*(y-ybar))
beta.hat = SSxy / SSxx
alpha.hat = ybar - beta.hat * xbar
alpha.hat   ### alpha.hat = 2608.884
beta.hat    ### beta.hat = -31.66814

model.matrix(model1)
X <- model.matrix(model1)
t(X) %*% X

solve( t(X) %*% X , t(X) %*% y)

X[,2] <- scale(X[,2], center = TRUE, scale = FALSE)
t(X) %*% X 
solve( t(X) %*% X , t(X) %*% y)

ybar
model1
detach(df)

###### Prediciting the future (using predict function) ######

olddata <- data.frame(Year = seq(1900,1996)) 
### Plotting the confidence interval ###
yhat <- predict(model1, olddata, interval = "confidence")
lines(olddata$Year, yhat[,"lwr"], lwd = 2)
lines(olddata$Year, yhat[,"upr"], lwd = 2)
### Plotting the prediction interval ###
yhat <- predict(model1, olddata, interval = "prediction")
lines(olddata$Year, yhat[,"lwr"], lty = 2)
lines(olddata$Year, yhat[,"upr"], lty = 2)

### Extrapolating new data using the predict function ###

newdata <- data.frame(Year = seq(1999,2010)) 

ytilde <- predict(model1, newdata, interval = "confidence")
lines(newdata$Year, ytilde[,"fit"], lwd = 2, col= "blue")
lines(newdata$Year, ytilde[,"lwr"], lwd = 2, col= "blue")
lines(newdata$Year, ytilde[,"upr"], lwd = 2, col= "blue")

ytilde <- predict(model1, newdata, interval = "prediction")
lines(newdata$Year, ytilde[,"lwr"], lty = 2, lwd = 2, col= "blue")
lines(newdata$Year, ytilde[,"upr"], lty = 2, lwd = 2, col= "blue")

#### To understand the sampling distribution of the regression coefficients ###
attach(df)
fit <- lm(Men200m ~ Year)
names(fit)
names(summary(fit))
sigma.hat <- summary(fit)$sigma

### Checking value of sigma hat with statistical computation ###

n <- nrow(df)
ehat <- residuals(fit)
s <- sum(ehat^2) / (n-2)
s <- sqrt(s)
s; sigma.hat ### Both values same sigma = 0.2981344

### We check the sampling distribution of alpha and beta ###
### and their effect on each other when values are centred around x ###

alpha = coef(fit)[1]
beta = coef(fit)[2]
x <- c(Year)

plot(0,0, type="n",
     xlim=c(1900,1995), ylim=c(19,23),
     xlab="Year", ylab="Time", 
     main= "Simulated Regression Lines") 

ab = c(0,0)
for(i in 1:1000) {
  yi <- rnorm(n, mean = (alpha + beta*x), sd = s)
  fiti <- lm(yi ~ x)
  abline(fiti, lty=2, col="grey")
  ab <- rbind(ab, coef(fiti))
}

ab <- ab[-1,]
dimnames(ab)[[2]] <- c("alpha.hat", "beta.hat")
plot(ab)

apply(ab, 2, sd)
summary(fit)
### Showing very strong correlation ###
cor(ab)

### Using values centred around (xi - x.bar) instead of x ###

x <- x - mean(x)

plot(0,0, type="n",
     xlim=c(-50,50), ylim=c(74,78),
     xlab="Year", ylab="Time", 
     main= "Simulated Regression Lines") 

### Simulating 1000 different regression lines ###

ab = c(0,0)
for(i in 1:1000) {
  yi <- rnorm(n, mean = (alpha + beta*x), sd = s)
  fiti <- lm(yi ~ x)
  abline(fiti, lty=2, col="grey")
  ab <- rbind(ab, coef(fiti))
}

ab <- ab[-1,]
dimnames(ab)[[2]] <- c("alpha.hat", "beta.hat")
plot(ab)

apply(ab, 2, sd)
### Showing weak correlation ###
cor(ab)
