# Example 17.1
load("mroz.RData")

### binary response model estimation
# family = "gaussian"
inlf.lpm <- glm(inlf ~ nwifeinc + educ + exper + expersq + age 
                + kidslt6 + kidsge6, family = "gaussian", data)

#linear regression
inlf.lpm.lm <- lm(inlf ~ nwifeinc + educ + exper + expersq + age 
                  + kidslt6 + kidsge6, data)

#probit, family = binomial
inlf.probit <- glm(inlf ~ nwifeinc + educ + exper + expersq + age 
                   + kidslt6 + kidsge6, family = "binomial"(link = "probit"), data)

#probit, family = binomial
inlf.logit <- glm(inlf ~ nwifeinc + educ + exper + expersq + age
                  + kidslt6 + kidsge6, family = "binomial"(link = "logit"), data)

#deviance residuals : sum of square of dr = deviance model
summary(inlf.logit)

### deviance
# deviance = sum of squared deviance residuals
summary(residuals(inlf.logit, type = "deviance"))

#sum of square deviance residual = equal to deviance of model -> deviance is equal to -2*loglikelyhood
sum(residuals(inlf.logit, type = "deviance")^2)
inlf.logit$deviance

# deviance = -2 * log likelihood
(-2) * logLik(inlf.logit)
#Z value = statistical value follows normal dist
#coefficient no longer tells the partial effect. 

### goodness of fit
# pseudo r-squared
1 - inlf.logit$deviance/inlf.logit$null.deviance

# percent correctly predicted
sum(abs(data$inlf - inlf.logit$fitted.values) <= 0.5)/nrow(data)

# information criteria
AIC(inlf.logit)
BIC(inlf.logit)


### LR test (LR = 2(Lur - Lr) ~ chi^2) (only particular betas = 0)

# test for overall significance
library(lmtest) #likelyhood function
lrtest(inlf.logit) #likelyhood ratio test (LR test)

## test for overall significance(all beta = 0, null deviance - residual deviance ~ chi^2)
LR.overall <- 1 - pchisq(inlf.logit$null.deviance - inlf.logit$deviance, 
                        df = inlf.logit$df.null - inlf.logit$df.residual)
#LR.overall = 0, because p-value is close to zero.

# hypothesis testing
library(car)
linearHypothesis(inlf.logit, c("exper = 0", "expersq = 0")) #it is also based on the chisq test

# confidence intervals
confint(inlf.logit)


### model selection
inlf.null <- glm(inlf ~ 1, family = "binomial"(link = "logit"), data)
inlf.full <- glm(inlf ~ nwifeinc + educ + exper + expersq + age
                + kidslt6 + kidsge6, family = "binomial"(link = "logit"), data)
inlf.fo <- step(inlf.null, scope = list(lower = inlf.null, upper = inlf.full), direction = "forward")
formula(inlf.fo)
inlf.bac <- step(inlf.full, direction = "backward")
formula(inlf.bac)

### prediction 
new.ob = data.frame(nwifeinc = 10.91, educ = 12, exper = 14, expersq = 14^2,
                     age = 32, kidslt6 = 1, kidsge6 = 0)

#this is beta0 + beta1 x1+ .... + beta_k xk
predict(inlf.logit, newdata = new.ob, type = "link")

#estimate probability
predict(inlf.logit, newdata = new.ob, type = "response")

#same as type = "link"
xb <- sum(cbind(1, new.ob) * inlf.logit$coefficients) #it means nothing. just input value

#same as type = "response"
phat <- exp(xb)/(1 + exp(xb)) #probablility


### interpretation of logit model
# partial effects in logit model
mean(inlf.logit$fitted.values * (1 - inlf.logit$fitted.values)) * inlf.logit$coefficients #slide 10 page
#nwifeinc = -0.003811 is 

# non-linear partial effects (only number of kids difference)
new.ob <- with(data, data.frame(nwifeinc = mean(nwifeinc), educ = mean(educ), exper = mean(exper),
                    expersq = mean(expersq), age = mean(age), kidslt6 = c(0, 1, 2), kidsge6 = 1))

# partial effect of having the first kid less than 6
predict(inlf.logit, newdata = new.ob[2, ], type = "response") - 
        predict(inlf.logit, newdata = new.ob[1, ], type = "response") # having the first kid -0.3456246


# partial effect of having the second kid less than 6
predict(inlf.logit, newdata = new.ob[3, ], type = "response") - 
        predict(inlf.logit, newdata = new.ob[2, ], type = "response")
