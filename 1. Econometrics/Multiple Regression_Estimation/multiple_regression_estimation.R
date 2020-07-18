install.packages("ggplot2")
library(ggplot2)
load("wage1.RData")
ls()  # show data sets and functions you have defined
desc
wage.data <- data

# Example 2.4
summary(wage.data)
ggplot(data = wage.data, aes(x = wage)) + geom_histogram()
ggplot(data = wage.data, aes(x = educ)) + geom_histogram() 
ggplot(data = wage.data, aes(x = educ, y = wage)) + geom_point()

linear.m1 <- lm(wage ~ educ, data = wage.data)
summary(linear.m1) 

# manual estimation
# OLS estimates
X <- cbind(rep(1, nrow(wage.data)), wage.data$educ)
y <- wage.data$wage
OLS.est <- solve(t(X) %*% X, t(X) %*% y)

attributes(linear.m1)

# r-squared
y.hat <- linear.m1$fitted.values
R.sqrd <- sum((y.hat - mean(y.hat))^2) / sum((y - mean(y))^2)
R.sqrd2 <- 1 - sum(linear.m1$residuals^2) / sum((y - mean(y))^2)

ggplot(data = wage.data, aes(x = educ, y = wage)) + geom_point() + stat_smooth(method = "lm")

# Example 3.2
log.m2 <- lm(lwage ~ educ + exper + tenure, data = wage.data)
summary(log.m2)
