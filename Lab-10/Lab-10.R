rm(list = ls())
count = c(42, 90, 134, 65, 99, 28, 19, 47, 72, 20, 61, 11)
P = c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
E = c(1,0,1,0,1,0,1,0,1,0,1,0)
S = c(1, 1, 2, 2, 3, 3, 1, 1, 2, 2, 3, 3)

df = data.frame(count, P, E, S)

mod1 = glm(count ~ factor(S) + factor(E) + factor(P), data = df, family = 'poisson')
mod2 = glm(count ~ factor(S)*factor(E) + factor(P), data = df, family = 'poisson')
mod3 = glm(count ~ factor(S)*factor(P) + factor(E), data = df, family = 'poisson')
mod4 = glm(count ~ factor(S) + factor(E)*factor(P), data = df, family = 'poisson')
mod5 = glm(count ~ factor(S)*factor(E) + factor(S)*factor(P), data = df, family = 'poisson')
mod6 = glm(count ~ factor(S)*factor(E) + factor(E)*factor(P), data = df, family = 'poisson')
mod7 = glm(count ~ factor(S)*factor(P) + factor(E)*factor(P), data = df, family = 'poisson')
mod8 = glm(count ~ factor(S)*factor(E) + factor(S)*factor(P) + factor(E)*factor(P), data = df, family = 'poisson')

deviance(mod1) ## 126.7325
deviance(mod2) ## 6.424935
deviance(mod4) ## 124.3386
deviance(mod5) ## 5.321362
deviance(mod7) ## 123.235
deviance(mod8) ## 2.775298
## Q8 ## (Complete Independence)

## H0: S, E and P are independent
## Ha: They are not independent

qchisq(0.95, df.residual(mod1)) ## 14.067
## since deviance > critical value (Reject H0)
pchisq(deviance(mod1), df.residual(mod1), lower=FALSE)
## p-val = 3.025*10^-24 (Model is a bad fit)

## Q9 ## (Block Independence)

## H0: E,P are associated jointly independent with S (EP + S)
## Ha: No evidence to prove the above

qchisq(0.95, df.residual(mod4)) ## 12.59159
## since deviance > critical val (Reject H0)
pchisq(deviance(mod4), df.residual(mod4), lower=FALSE)
## p-val = 1.9966*10^-24 (Model is a bad fit)

## Q10 ## (Partial Independence)

## H0: S,P are partially independent with E (SE + PE)
## Ha: No evidence to prove the above

qchisq(0.95, df.residual(mod6)) ## 9.487729
## since deviance < critical val (Accept H0)
pchisq(deviance(mod6), df.residual(mod6), lower=FALSE)
## p-val = 0.401819 (Model is a good fit)

## Q11 ## (Uniform Association)

## H0: S,P,E are all associated (SP + EP + SE)
## Ha: No evidence to prove the above

qchisq(0.95, df.residual(mod8)) ## 5.991465
## since deviance < critical val (Accept H0)
pchisq(deviance(mod8), df.residual(mod8), lower=FALSE)
## p-val = 0.2496616 (Model is a good fit)