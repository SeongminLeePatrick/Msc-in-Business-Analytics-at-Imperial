library(ggplot2)

#### Outlier
load("rdchem.RData")
rdchem.m1 <- lm(rdintens ~ sales + profmarg, data)
summary(rdchem.m1)

#check outlier
#dependent var outlier (recaled, above and below 3 standard deviate are outlier. we can't see any outlier in the dependent var)
ggplot(rdchem.m1, aes(.fitted, .stdresid)) + geom_point() + stat_smooth(method = "loess") + xlab("Fitted Value") + ylab("Standardized Residuals")

#cookdistance (2(k+1)/n = 2(2+1)/32 = 0.2), we can see 1 point more than 0.2
ggplot(rdchem.m1, aes(.hat, .cooksd)) + geom_point() + stat_smooth(method = "loess") + xlab("Leverage") + ylab("Cook's Distance")

# leverage value (from hat matrix) x(xTx)-1XT : 10이 높음 
hatvalues(rdchem.m1)

# cooks distance: 10이 높음 
cooks.distance(rdchem.m1)

data.new <- data[-10, ]
rdchem.m2 <- lm(rdintens ~ sales + profmarg, data.new)