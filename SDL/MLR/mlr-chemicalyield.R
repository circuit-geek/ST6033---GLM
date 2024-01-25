rm(list=ls())
df <- data.frame(Y = c(418.48, 417.03, 285.23, 318.86, 186.27, 286.60, 78.927, 103.57, 185.26, 216.20, 371.29, 162.48, 410.48, 418.34, 331.68, 274.78, 336.88, 362.53, 328.90, 234.79, 128.19, 485.29, 157.25, 240.55, 96.148, 228.42, 413.56, 272.38, 263.14, 94.099),
                 X1 = c(89, 97, 63, 77, 41, 70, 23, 25, 51, 53, 85, 38, 93, 94, 69, 39, 64, 68, 60, 49, 15, 89, 19, 42, 12, 36, 85, 53, 44, 2),
                 X2 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)  # Removed the extra comma here
)

plot(df)

plot(df$X1, df$Y, col = ifelse(df$X2 == 1, "black", "red"),
     pch = 16, main = "Scatterplot of Chemical Process Yield",
     xlab = "Concentration", ylab = "Percipitate Yield g/hour")

legend("topright", legend = c(0, 1),
       col = c("red", "black"), pch = 16, cex = 0.8)

mod <- lm(Y ~ X1 + X2, data = df)
summary(mod)
residuals(mod)
residuals <- residuals(mod)

plot(fitted(mod), residuals, col = ifelse(df$X2 == 1, "black", "red"),
     pch = 16, main = "Residuals vs. Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals")

legend("topright", legend = c(0, 1),
       col = c("red", "black"), pch = 16, cex = 0.8)

qqnorm(residuals, col = ifelse(df$X2 == 1, "black", "red"))

legend("topright", legend = c(0, 1),
       col = c("red", "black"), pch = 16, cex = 0.8)
anova(mod)
