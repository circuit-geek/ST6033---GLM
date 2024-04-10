rm(list=ls())
Age = c(1, 2, 3, 4)
Using = c(72, 105, 237, 93)
n = c(397, 404, 612, 194)
df = data.frame(Age, Using, n)

log_model = glm(cbind(Using, n-Using) ~ as.factor(Age), data = df,
                family = 'binomial')
summary(log_model)

## On observing the Dev Residuals we can see that it is the 
## saturated model which we have tried to fit.

## for age group 1 => -1.5072
## for age group 2 => -1.5072 + 0.4607(1) = -1.0465
## for age group 3 => -1.5072 + 1.0483(1) = -0.4589
## for age group 4 => -1.5072 + 1.4286(1) = -0.0786

## Calculating the odds

exp(-1.5072) ## 0.2215294
exp(-1.0465) ## 0.3511647
exp(-0.4589) ## 0.6319784
exp(-0.0786) ## 0.9244096

## Calculating odds ratio for age group 3 to 1

odds_ratio = 0.6319784/0.2215294
odds_ratio ## => 2.852797

## Interpretation for this odds ratio is the age group of 3 (30-39)
## use contraception more than group 1 (<25) by 185% more.

## odds ratio from estimate is the exp(estimate).

exp(1.0483) ## => 2.852797 (p-value => 1.14*10^-11)

# Since p-value is less than 0.05 it is statistically significant
# It means that there is a significant difference in the use the
# of contraception between the age group < 25 and age group (30-39)

AgeC<-c(20, 27.5, 35,45)

log_model_2 = glm(cbind(Using, n-Using) ~ AgeC, family = 'binomial')
summary(log_model_2)
exp(0.060671)
## Yes the coefficent associate with age is statistically significant
## that for 1 year increase in age the odds of using 
## contraception is 6% more.
 
exp(confint(log_model_2)) #[1.047, 1.077]

## H0: Null hypothesis states that there is no difference in odds of using contraception
## among different age groups.
## From confint we know 1 doesn't lie in the range. So reject H0. 