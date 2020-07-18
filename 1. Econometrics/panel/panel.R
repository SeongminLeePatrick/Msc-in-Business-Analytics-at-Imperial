# Example 9.7. City crime rates
load("crime2.RData")
crime.87 <- lm(crmrte ~ unem, data, subset = year == 87)
summary(crime.87) # not statistically significant
crime.pool <- lm(crmrte ~ unem + factor(year), data)
summary(crime.pool) #coef of unem is insignificant as well.

# estimation with plm (defferenciate variable / time series)
install.packages("plm")
library(plm)

# create a panel data frame
data$city <- rep(1:46, each = 2) #based on the index, which city this belongs to
head(data)

#panel data frame + index(cross-sectional unit, time series unit)
data.p <- pdata.frame(data, index = c("city", "year"))

# index of a panel
index(data.p)

# dimensions of a panel
pdim(data.p) #Balanced Panel: n=46(#of city), T=2(each city has 2 penal), N=92 (total obsv)

# panel data operations
head(data.p$unem, 10) #cross-sectional data by which city which year
head(diff(data.p$unem), 10) #different from adject obeservations. -4.5 means the difference of unemp b/t 1-82 and 1-87 
head(lag(data.p$unem), 10) #lag of ump, 8.2 = the unmp rate of city 1 in 1982.

# first difference estimation
crime.fd <- plm(crmrte ~ d87 + unem, data, index = c("city", "year"), 
                effect = "individual", model = "fd") #model = 'fd' : first difference
summary(crime.fd) #residuals : difference of variables's residuals, not variable itself residual
#coef of original time variable is intercept of the model above.
#difference variable's TSS, RSS, Rsqr, not variable itself.


# Example 14.1. Effect of job training on firm scrap rates
load("jtrain.RData")
scrap.fe <- plm(log(scrap) ~ grant + grant_1 + d88 + d89, data, 
                index = c("fcode", "year"), effect = "individual", model = "within")

scrap.fd <- plm(log(scrap) ~ 0 + grant + grant_1 + d88 + d89, data, 
                index = c("fcode", "year"), effect = "individual", model = "fd")

# test for autocorrelation
pwartest(scrap.fe)