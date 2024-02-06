rm(list = ls())
### Performing one-way anova to compare means of various groups

### Input the data ###
low = c(54.7, 50.5, 45.9 ,59.7 ,51.5, 45.5, 47.4, 
        55.6, 59.4 ,56.9)
inter = c(48.6, 55.8, 62.6, 64.7 ,54.3, 61.3,
          44.6, 59.8 ,46.0, 56.5,42.4 ,71.4, 62.6, 67.9,
          55.2, 47.3, 75.9)
high=c(63.8 ,65.4, 68.1, 70.0, 63.9, 63.6, 63.6,
       59.5, 56.3 ,59.1, 60.6)

### combining the three vectors into one single vector ###
y = c(low, inter, high)

### Creating a response variable called Grade (Grade of Tumor) ###
grade = c(low*0+1, inter*0+2, high*0+3)

### Performing exploratory plot and interpreting the output
plot(grade, y, xlab = "Grade of Tumor", ylab = "Metabolic Rate",
     pch=20, cex=2)

### From the above plot we can infer the following:
### a) The MR is higher for grade 3 compared to the other 2 grades
### b) The MR is least for grade 1
### c) There is more variability in MR in grade 2 compared to the other 2 grades.

### Plotting boxplot
boxplot(y~grade)

### From the boxplot we can intrepret the following:
### a) From grade 1 and 2 the boxplot median is almost in the middle
### showing evidence of symmetric data, where data for grade 3 is more skewed.
### b) Also same inference has scatterplot where more variability is found in grade 2.
### c) Since there are no points outside the boxplot, no evidence for outliers.

### Performing one-way anova ###
ow.aov = aov(y ~ as.factor(grade))
summary(ow.aov)

### From the summary of one way anova we can see that the p-value
### is significant at the 5% level, less than 0.05. Therefore,
### we can reject the H0: Means are equal for all three grades.
### and we accept the Ha: There is statistically difference between 
### the three grades.

### We can use a multiple linear regression model, for this analysis
### The assumptions made are that the errors are normally distributed
### with mean = 0 and constant variance = sigma.squared, and they are 
### independent.

### Plotting the model residuals by grade
plot(grade, ow.aov$residuals, ylab = "Residuals")

### From the residual plot it is very clear that grade 2 residuals
### are not having constant variance, there is a lot of variability.

### plotting the QQ-Plot
qqnorm(ow.aov$residuals)
qqline(resid(ow.aov))

### From the qqplot though there are some significant deviations
### from the refernce line in the beginning and at the end, 
### still majority of the residuals seems to follow the normal
### distribution, this can further be verified by plotting a
### histogram of the residuals to show the symmetric bell curve.

hist(ow.aov$residuals, freq = FALSE)
curve(dnorm(x, mean = 0, sd = 10),
      from = -30,
      to = 30,
      add = TRUE,
      col = "red", lwd=4)

### Almost symmetric, we can conclude the residuals are 
### normally distributed.

### Performing one-way anova analysis using normal linear model
### creating indicator variables for grade 2 and grade 3.

x2 = ifelse(grade == 2, 1, 0)
x3 = ifelse(grade == 3, 1, 0)

cbind(y, grade, x2, x3)

### Fitting linear model with y as response variable 
### and x2 and x3 as predictor variables.

model = lm(y ~ x2 + x3)
summary(model)

### Interpreting the model coefficients and output
### Beta.0 shows the mean MR for low grade, when x2 and x3 are 0.
### Beta.1 is the mean difference in MR for inter and low grade
### Beta.2 is the mean difference in MR for high and low grade
### on checking the p-value for x2 and x3.
### The H0 for Beta.2 = 0 can be accepted, since it is not 
### significant at the 5% level.
### between low grade and inter grade is not statistically different.
### Whereas The H0 for Beta.3 = 0 can be reject, since the p-value
### is less that 0.05.
### Which mean there is statistical difference, and high grade is higher
### than low grade by 10.372.

model2 = lm(y ~ 1)
summary(model2)

### This intercept represents that there is no mean difference
### between all three grades. (It's the null model).