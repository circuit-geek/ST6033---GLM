rm(list=ls())
attach(InsectSprays)
names(InsectSprays)
df = InsectSprays

## With increase in mean there is also increase in variance
## data is skewed
## evidence for poisson like data
boxplot(count~spray)

poi_model = glm(count ~ spray, data = df, family = 'poisson')

summary(poi_model)

## Interpretation of Intercept: It is the log mean of sprayA
## ie: e^2.67415 = 14.5 => Mean of sprayA

## Interpretation of SprayB: It is the increase log mean of spray B
## with respect to SprayA. log mean of sprayB is higher than A
## by 0.05588 ie: e^0.05588 = 1.06. The expected mean of sprayB is 6%
## higher than sprayA. Since the p-value of B is 0.597.
## The Null Hypothesis (H0: There is no significant difference in effect between A and B)
## The Alternative Hypothesis (Ha: There is a significant difference in effect between the two)
## since the p-value is greater than 5% significance level, we fail to reject H0.
## and we conclude that there is no statisical significance to prove that they are different.

## Going by the estimated coefficients sprayC has the highest reduction
## in log mean with respect to sprayA, the log mean of sprayC is lesser than A
## by 1.94018. e^-1.94018


pchisq(deviance(poi_model), df.residual(poi_model), lower = FALSE) 
## p-value => 0.00605 - since p - value is less 0.05 we reject the (H0: The PM and SM are not different)
## and accept (Ha: The PM and SM are different), since this value is statistically significant
## it suggest that model is not fitting the data well.