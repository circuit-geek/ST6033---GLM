rm(list=ls())
setwd("D:/UCC/Academic Work/Sem-2/ST6033 - GLM/SDL/MLR")
df <- read.csv("cheese.csv", header = TRUE)
attach(df)
pairs(df)
cor(df)

fit.Acetic <- lm(Taste ~ Acetic)

fit.H2S <- lm(Taste ~ H2S)

fit.Lactic <- lm(Taste ~ Lactic)

par(mfrow=c(1,3))

plot( x = Acetic , y = Taste , 
      
      xlab="Acetic Acid", ylab="Taste Score", main="Acetic",
      
      panel.last = lines(sort(Acetic), fitted(fit.Acetic)[order(Acetic)]) )

title(sub= paste("R-squared = ", round( summary(fit.Acetic)$r.squared * 100, 1),
                 
                 "% , sigma = ", round( summary(fit.Acetic)$sigma, 3) ) ) 



plot( x = H2S , y = Taste , 
      
      xlab="H2S", ylab="Taste Score", main="H2S",
      
      panel.last = lines(sort(H2S), fitted(fit.H2S)[order(H2S)]) )

title(sub= paste("R-squared = ", round( summary(fit.H2S)$r.squared * 100, 1),
                 
                 "% , sigma = ", round( summary(fit.H2S)$sigma, 3) ) ) 



plot( x = Lactic , y = Taste , 
      
      xlab="Lactic Acid", ylab="Taste Score",
      
      main="Lactic",
      
      panel.last = lines(sort(Lactic), fitted(fit.Lactic)[order(Lactic)]) )

title(sub= paste("R-squared = ", round( summary(fit.Lactic)$r.squared * 100, 1),
                 
                 "% , sigma = ", round( summary(fit.Lactic)$sigma, 3) ) ) 

### On inspecting the individual models H2S has the best
### R.squared = 0.571 and with sigma = 10.833

AIC(fit.Acetic) ### 246.6389
AIC(fit.H2S) ### 232.0245
AIC(fit.Lactic) ### 236.8724

sm <- lm(Taste~Acetic+H2S+Lactic)
summary(sm)
AIC(sm) ### 229.7775
step(sm)

### The best model from the step function is ###
bm <- lm(Taste ~ H2S + Lactic)
AIC(bm) ### 227.7838
summary(bm) ### Adj R.squared = 0.6259
