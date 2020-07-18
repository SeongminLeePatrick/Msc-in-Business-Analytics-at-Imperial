install.packages("sandwich")
install.packages("lmtest")
library(sandwich) #robust error calculation
library(lmtest)

#### BP test and White test
load("wage1.RData")
wage.m1 <- lm(log(wage) ~ educ + exper + tenure, data)
wage.m2 <- lm(wage ~ educ + exper + tenure, data)

# residual plot (check if variance of residual should be the same.)
library(ggplot2)
ggplot(wage.m1, aes(.fitted, .resid)) + geom_point() + geom_hline(yintercept = 0)
ggplot(wage.m2, aes(.fitted, .resid)) + geom_point() + geom_hline(yintercept = 0)

# BP test(lmtest package) #focus on just p-value
# result : BP = 10.761, df = 3, p-value = 0.01309 : reject the null(homoscasticity)
bptest(wage.m1)

bptest(wage.m2) #This pvalue is way smaller than log range. variation is bigger

#log trasformation -> mitigation of Y variation

# White test #focus on just p-value
fitted.wage <- wage.m1$fitted.values
rsq_wh <- wage.m1$residuals^2
ex1 <- lm(rsq_wh ~ fitted.wage + I(fitted.wage^2), data)
ex1.rsq <- summary(ex1)$r.squared
ex1.F <- (ex1.rsq/2)/((1-ex1.rsq)/wage.m1$df.residual)
1 - pf(ex1.F, 2, wage.m1$df.residual)

#result: BP = 6.6953, df = 2, p-value = 0.03517 : reject the null(homoscasticity)
bptest(wage.m1, ~ fitted.wage + I(fitted.wage^2))

# Running BP test manually
res.sq <- wage.m1$residuals^2 
aux <- lm(res.sq ~ educ + exper + tenure, data)
aux.rsq <- summary(aux)$r.squared
aux.F <- (aux.rsq/3) / ((1 - aux.rsq) / wage.m1$df.residual)
1 - pf(aux.F, 3, wage.m1$df.residual)


#### robust t test and F test
# robust vcov
vcov.robust <- vcovHC(wage.m1, "HC1")

# t test (using standard error)
coeftest(wage.m1, vcov = vcov.robust)
summary(wage.m1) # To compare

# F test
library(car)
linearHypothesis(wage.m1, "educ - exper = 0", white.adjust = "hc1") #hc1, hc2, hc2,, all can be used
