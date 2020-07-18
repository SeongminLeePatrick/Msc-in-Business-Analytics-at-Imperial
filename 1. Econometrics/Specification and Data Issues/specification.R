#### Functional form misspecifation
library(lmtest)
load("hprice1.RData")

# RESET test
house.m1 <- lm(price ~ lotsize + sqrft + bdrms, data)
resettest(house.m1, type = "fitted") # Y hat can be used , reject the H0 because pvalue is below 0.05
house.m2 <- lm(log(price) ~ log(lotsize) + log(sqrft) + bdrms, data)
resettest(house.m2, type = "fitted") # we can't reject H0 -> this model is more proper than 1st model

# Davidson-MacKinnon test
lprice.m1 <- lm(log(price) ~ lotsize + sqrft + bdrms, data)
lprice.m2 <- lm(log(price) ~ log(lotsize) + log(sqrft) + bdrms, data)
jtest(lprice.m1, lprice.m2)
#result: M1 + fitted(M2) tells that coefficeint of fitted m2 is zero.
# M2 + fitted(M1) tells that no rejection of H0, therefore, prepfer Model2
# 'Estimate' is the cofficient of fitted(M1/2)


#### Variable Selections
install.packages("leaps")
library(leaps)
load("bwght.RData")
data.new <- na.omit(data)
bwght.model.search <- regsubsets(bwght ~ faminc + fatheduc + motheduc + parity 
                                 + male + white + cigs, data.new, nbest = 3) #adjusted r2 관점에서 모든조합, nbset = 3 of best performance model with 1 variable, 2 variable, 3 variables
summary(bwght.model.search)

plot(bwght.model.search, scale = "adjr2") #R2 관점에서 plot으로 볼 때 1variable중 3개의 best보여주고, 2 var 3개 베스트 보여주고, adjusted r square 0.049 r^2에서 모든변수 포함,  (블랙이면 포함됨, 흰색이면 빠짐)
plot(bwght.model.search, scale = "bic")#y축은 bic임, each one is rank, lower is better, therefore top one is best, which is moth+party+male+white+cigs임 

# stepwise search 
bwght.null <- lm(bwght ~ 1, data.new) # only include intercept
bwght.full <- lm(bwght ~ faminc + fatheduc + motheduc + parity + male + white 
                 + cigs, data.new) # include all var
#AIC
step(bwght.null, scope = list(lower = bwght.null, upper = bwght.full), 
     direction = "forward")# bwght ~ 1에서 what if we add var not in the model, -> if add cigs, then AIC 7122 / if add only male, 

step(bwght.full, data.new, direction = "backward") #start with full vari / backward and forward can have different model

step(bwght.null, scope = list(lower = bwght.null, upper = bwght.full), 
     direction = "both") # add a new one which not in the model, and remove the var which already in the model

#BIC
n <- nrow(data.new)
step(bwght.null, scope = list(lower = bwght.null, upper = bwght.full), 
     direction = "forward", k = log(n)) #k = log(n) -> evaluate BIC


#### Prediction
load("wage1.RData")
wage.m1 <- lm(wage ~ educ + exper, data)

newdata <- data.frame(educ = 12, exper = 8)
predict(wage.m1, newdata, interval = "confidence", level = 0.95) #95% confidence interval fit 4.54 ~ 5.25
predict(wage.m1, newdata, interval = "predict", level = 0.95) #much wider interval, 

wage.m2 <- lm(log(wage) ~ educ + exper, data)
predicted.logwage <- predict(wage.m2, newdata, interval = "none")
predicted.wage <- mean(exp(wage.m2$residuals)) * exp(predicted.logwage)
predicted.wage
