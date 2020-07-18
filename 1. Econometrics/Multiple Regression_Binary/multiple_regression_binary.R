library(dplyr)
library(car)
library(ggplot2)

# Example 7.7
load("beauty.RData")
desc
data.male <- data %>% filter(female == 0)
male.m1 <- lm(log(wage) ~ belavg + abvavg + educ + exper, data = data.male)
data.female <- data %>% filter(female == 1)
female.m1 <- lm(log(wage) ~ belavg + abvavg + educ + exper, data = data.female)
stargazer(male.m1, female.m1, no.space = TRUE, align = TRUE)

# Interactions involving dummy variables
load("wage1.RData")
wage.m1 <- lm(log(wage) ~ female + educ + female:educ, data = data)
summary(wage.m1) #individually not significant
linearHypothesis(wage.m1, c("female = 0", "female:educ = 0")) #jointly significant
#The result says that gender really has a impact on the wage because it is correlated.

#test the correlation
data$female.educ <- data$female * data$educ
cor(data$female.educ, data$female) 
#What are the implication of correlation? 
#0 is male education level, the others are the education level of female.
#smaller the variation of the education level, the higher the correlation b/t two is

summary(data.female$educ) #1st Qu: 12, and 3rd Qu: 13,... -> small variation -> highly correlated
ggplot(data = data.female, aes(x = educ)) + geom_histogram()

# Labor force participation by married women (dependent variable = 0 or 1)
load("mroz.RData")
fitted.inlf <- lm(inlf ~ educ + kidslt6, data = data)
summary(fitted.inlf)
