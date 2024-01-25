rm(list=ls())
### Anaerobic = 0, Aerated = 1
df <- data.frame(
  Age = c(2, 5, 8, 10, 14, 15, 18, 2, 5, 8, 10, 14, 15, 18),
  Bacteria = c(1319, 1434, 1523, 1656, 1834, 1988, 2296, 1516, 1989, 2345, 2732, 3118, 3435, 3787),
  Culture = c(0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1)
)

plot(df$Age, df$Bacteria, col = ifelse(df$Culture == 1, "black", "red"),
     pch = 16, main = "Scatterplot of Bacteria Cultures",
     xlab = "Age", ylab = "Bacteria")

legend("topright", legend = c(0, 1),
       col = c("red", "black"), pch = 16, cex = 0.8)

mod1 <- lm(Bacteria ~ Age + Culture + Age*Culture, data = df)
summary(mod1)
anova(mod1)
summary.aov(mod1)

mod2 <- lm(Bacteria ~ Age, data = df)
summary(mod2)

residuals <- residuals(mod1)

plot(fitted(mod1), residuals, col = ifelse(df$X2 == 1, "black", "red"),
     pch = 16, main = "Residuals vs. Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals")
legend("topright", legend = c(0, 1),
       col = c("red", "black"), pch = 16, cex = 0.8)

qqnorm(residuals, col = ifelse(df$X2 == 1, "black", "red"))
legend("topright", legend = c(0, 1),
       col = c("red", "black"), pch = 16, cex = 0.8)
par(mfrow=c(2,2))
plot(mod1)





