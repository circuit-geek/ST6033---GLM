rm(list=ls())
library("daewr")
setwd("D:/UCC/Academic Work/Sem-2/ST6033 - GLM/Lab/ST6033---GLM/SDL/Poisson Regression")
gala = read.table("gala.txt")

gala <- gala[,-2]
modp <- glm(Species ~ .,family=poisson,gala)
plot(modp)


plot(residuals(modp) ~ predict(modp,type="response"),xlab=expression(hat(mu)),ylab="Deviance residuals")

plot(residuals(modp) ~ predict(modp,type="link"),xlab=expression(hat(eta)),ylab="Deviance residuals")

plot(residuals(modp,type="response") ~ predict(modp,type="link"),xlab=expression(hat(eta)),ylab="Response residuals")

plot(Species ~ Area, gala)

plot(Species ~ log(Area), gala)

mu <- predict(modp,type="response")
z <- predict(modp)+(gala$Species-mu)/mu

plot(z ~ log(Area), gala,ylab="Linearized Response")

modpl <- glm(Species ~ log(Area) + log(Elevation) + log(Nearest) + log(Scruz+0.1) + log(Adjacent), family=poisson, gala)

c(deviance(modp),deviance(modpl))

mu <- predict(modpl,type="response")

u <- (gala$Species-mu)/mu + coef(modpl)[2]*log(gala$Area)

plot(u ~ log(Area), gala,ylab="Partial Residual")

abline(0,coef(modpl)[2])
     
