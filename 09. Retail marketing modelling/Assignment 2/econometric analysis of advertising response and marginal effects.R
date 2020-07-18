library(tidyverse)
library(readxl)
library(tseries)
library(lubridate)

############ read data 

data <- read_excel("DB_CarQuotes_Weekly_RMA.xlsx", sheet = 1)[-c(100:116),]
# summary(data)

# transform to numeric variables
cols = c("Lead Brochure Car1+NewCar1", "Lead Quotation Car1+NewCar1", "Lead TD Car1+NewCar1", 
         "Investment ON Car1", "Investment TV Car1 Copy1", "Investment TV Car1 Copy2", 
         "Investment TV Car1 Copy3", "Investment TV Car1 Copy4")    
data[,cols] = apply(data[,cols], 2, function(x) as.numeric(x))

# introduce a lag variable for Quotations and Orders
data$lag.quotations <- c(NA, head(data$`Quotations Car1+NewCar1`, -1))
data$lag.orders <- c(NA, head(data$`Orders Car1+NewCar1`, -1))

# create new variable of total investment / Total GRP radio
data <- data %>% mutate("TOT Investment Radio" = `Investment Radio 1`+`Investment Radio 2`+`Investment Radio 3`,
                        "GRP Radio" = `GRP RADIO 1` + `GRP RADIO 2` + `GRP RADIO 3`)

# create a new variable "Other Offline Activities" (reduced by TV and Radio investment)
data <- data %>% mutate("Other Offline Activities" = `Investment OFF Car1` - `TOT Investment Radio` - `Investment TV Car1`)


############ time series decomposition 

# create time series for orders & quotations to extract seasonality + trend
# we use freq = 4 to detect any patterns from month to month (approx.)
ts_orders <- ts(data$`Orders Car1+NewCar1`, frequency = 4)
ts_quotation <- ts(data$`Quotations Car1+NewCar1`, frequency = 4)

# plot(stl(ts_orders,'periodic'))
plot(stl(ts_quotation,'periodic'))

############ unit root test 

# Before we run the regressions, we test the variables for unit roots. While there is a 
# unit root in the dependent variable, all independent variables pass the tests (no unit root)
# so we can proceed with the regressions.

# ADF says there is a unit root, the other two tests say no
# for Orders two of three test confirm that there is unit root 
adf.test(data$`Orders Car1+NewCar1`) # not rejected - unit root
pp.test(data$`Orders Car1+NewCar1`) # rejected - stationary
kpss.test(data$`Orders Car1+NewCar1`) # rejected - unit root

# for Quotations two of three tests decline that there is unit root 
adf.test(data$`Quotations Car1+NewCar1`) # not rejected - unit root (at 5% level, at 10% level ok)
pp.test(data$`Quotations Car1+NewCar1`) # rejected - stationary
kpss.test(data$`Quotations Car1+NewCar1`) # rejected - unit root

# check whether independent variables have a unit root
adf.test(data$`TOT Investment Radio`) # rejected - stationary
pp.test(data$`TOT Investment Radio`) # rejected - stationary
kpss.test(data$`TOT Investment Radio`) # not rejected - stationary

adf.test(data$`Investment TV Car1`) # rejected - stationary
pp.test(data$`Investment TV Car1`) # rejected - stationary
kpss.test(data$`Investment TV Car1`) # not rejected - stationary

adf.test(data$`Other Offline Activities`) # rejected - stationary
pp.test(data$`Other Offline Activities`) # rejected - stationary
kpss.test(data$`Other Offline Activities`) # not rejected - stationary (at 5% level)

adf.test(data$`Investment ON Car1`) # rejected - stationary
pp.test(data$`Investment ON Car1`) # rejected - stationary
kpss.test(data$`Investment ON Car1`) # not rejected - stationary

adf.test(data$`Investment Search Car1`) # not rejected - unit-root
pp.test(data$`Investment Search Car1`) # rejected - stationary
kpss.test(data$`Investment Search Car1`) # not rejected - stationary (at 5% level)

adf.test(data$`Investment Programmatic Car1`) # rejected - stationary
pp.test(data$`Investment Programmatic Car1`) # rejected - stationary
kpss.test(data$`Investment Programmatic Car1`) # not rejected - stationary (at 5% level)

adf.test(data$`Investment TOT UmbrellaBrand`) # rejected - stationary
pp.test(data$`Investment TOT UmbrellaBrand`) # rejected - stationary
kpss.test(data$`Investment TOT UmbrellaBrand`) # not rejected - stationary

adf.test(data$`Investment TOT UmbrellaBrand OtherModels`) # not rejected - unit root (very close to 5% level)
pp.test(data$`Investment TOT UmbrellaBrand OtherModels`) # rejected - stationary
kpss.test(data$`Investment TOT UmbrellaBrand OtherModels`) # not rejected - stationary

adf.test(data$`Investment TOT DirectCompetitor1+DirectCompetitor2`) # rejected - stationary 
pp.test(data$`Investment TOT DirectCompetitor1+DirectCompetitor2`) # rejected - stationary
kpss.test(data$`Investment TOT DirectCompetitor1+DirectCompetitor2`) # not rejected - stationary


########## regression 
# note: controlling for copetitors not significant

# model 1 linear response
m1 <- lm(`Quotations Car1+NewCar1` ~ `GRP Radio` + `GRP TV Car1` + 
           `Other Offline Activities` + `Investment ON Car1` + `Investment Search Car1` +
           `Investment Programmatic Car1` + `Investment TOT UmbrellaBrand` + 
           `Investment TOT UmbrellaBrand OtherModels` +
           `Dummy August` + `Dummy Holidays` + `Dummy NewCar1 Full Launch` + 
           `Dummy OpenDoors Car1` + `Dummy OpenDoors Launch NewCar1` + `Dummy RunOut Car1`+
           `Investment TOT DirectCompetitor1+DirectCompetitor2`, data = data[-1,])
summary(m1)

# model 2 concave response
m2 <- lm(`Quotations Car1+NewCar1` ~ log(`GRP Radio`+1) + log(`GRP TV Car1`+1) + 
           log(`Other Offline Activities`+1) + log(`Investment ON Car1`+1) + log(`Investment Search Car1`+1) +
           log(`Investment Programmatic Car1`+1) + log(`Investment TOT UmbrellaBrand`+1) + 
           log(`Investment TOT UmbrellaBrand OtherModels`+1) + `Dummy August` +
           `Dummy Holidays` + `Dummy NewCar1 Full Launch` + 
           `Dummy OpenDoors Car1` + `Dummy OpenDoors Launch NewCar1` + `Dummy RunOut Car1`+
           `Investment TOT DirectCompetitor1+DirectCompetitor2`, data = data[-1,])
summary(m2)

# model 3 concave-quadratic response
m3 <- lm(`Quotations Car1+NewCar1` ~ `GRP Radio` + I(`GRP Radio`^2) + 
           `GRP TV Car1` + I(`GRP TV Car1`^2) +`Other Offline Activities` + 
           I(`Other Offline Activities`^2) + `Investment ON Car1` + I(`Investment ON Car1`^2) + 
           `Investment Search Car1` +I(`Investment Search Car1`^2) + `Investment Programmatic Car1`+
           I(`Investment Programmatic Car1`^2) + `Investment TOT UmbrellaBrand` + 
           I(`Investment TOT UmbrellaBrand`^2) + `Investment TOT UmbrellaBrand OtherModels` + 
           I(`Investment TOT UmbrellaBrand OtherModels`^2) + `Dummy August` + `Dummy Holidays` +
           `Dummy NewCar1 Full Launch` + `Dummy OpenDoors Car1` + `Dummy OpenDoors Launch NewCar1` + 
           `Dummy RunOut Car1` + `Investment TOT DirectCompetitor1+DirectCompetitor2`, data = data[-1,])

summary(m3)

# model 4 linear response with carryover
m4 <- lm(`Quotations Car1+NewCar1` ~ `GRP Radio` + `GRP TV Car1` + 
           `Other Offline Activities` + `Investment ON Car1` + `Investment Search Car1` +
           `Investment Programmatic Car1` + `Investment TOT UmbrellaBrand` + 
           `Investment TOT UmbrellaBrand OtherModels` +
           `Dummy August` + `Dummy Holidays` + `Dummy NewCar1 Full Launch` + 
           `Dummy OpenDoors Car1` + `Dummy OpenDoors Launch NewCar1` + `Dummy RunOut Car1` +
           lag.quotations + `Investment TOT DirectCompetitor1+DirectCompetitor2`, data = data)
summary(m4)

# model 5 concave response with carryover
m5 <- lm(`Quotations Car1+NewCar1` ~ log(`GRP Radio`+1) + log(`GRP TV Car1`+1) + 
           log(`Other Offline Activities`+1) + log(`Investment ON Car1`+1) + log(`Investment Search Car1`+1) +
           log(`Investment Programmatic Car1`+1) + log(`Investment TOT UmbrellaBrand`+1) + 
           log(`Investment TOT UmbrellaBrand OtherModels`+1) +
           `Dummy August` + `Dummy Holidays` + `Dummy NewCar1 Full Launch` + 
           `Dummy OpenDoors Car1` + `Dummy OpenDoors Launch NewCar1` + `Dummy RunOut Car1`+
           lag.quotations + `Investment TOT DirectCompetitor1+DirectCompetitor2`, data = data)
summary(m5)

# model 6 concave-quadratic response with carryover
m6 <- lm(`Quotations Car1+NewCar1` ~ `GRP Radio` + I(`GRP Radio`^2) + 
           `GRP TV Car1` + I(`GRP TV Car1`^2) +`Other Offline Activities` +
           I(`Other Offline Activities`^2) + `Investment ON Car1` + I(`Investment ON Car1`^2) + `Investment Search Car1` + 
           I(`Investment Search Car1`^2) +`Investment Programmatic Car1`+I(`Investment Programmatic Car1`^2) + 
           `Investment TOT UmbrellaBrand` + I(`Investment TOT UmbrellaBrand`^2) + `Investment TOT UmbrellaBrand OtherModels` + 
           I(`Investment TOT UmbrellaBrand OtherModels`^2) + `Dummy August` + `Dummy Holidays` + 
           `Dummy NewCar1 Full Launch` + `Dummy OpenDoors Car1` + `Dummy OpenDoors Launch NewCar1` + 
           `Dummy RunOut Car1`+ lag.quotations + `Investment TOT DirectCompetitor1+DirectCompetitor2`, data = data)
summary(m6)

# best model: m4 adj R^2 = 0.892 
# significant: GRP Radio, GRP TV, Search, (Programmatic), UmbrellaBrand OtherModels, 
# Dummy August, New Car1 Full Launch, Open Doors Launch, lag

# final model fit
plot(data$`Quotations Car1+NewCar1`[-1], m4$fitted, ylim=c(0,9000),
     xlim=c(0,9000),xlab='Quotations',ylab='Predictions')

# model 1 linear response
m1.o <- lm(`Orders Car1+NewCar1` ~ `GRP Radio` + `GRP TV Car1` + 
             `Other Offline Activities` + `Investment ON Car1` + `Investment Search Car1` +
             `Investment Programmatic Car1` + `Investment TOT UmbrellaBrand` + 
             `Investment TOT UmbrellaBrand OtherModels` +
             `Dummy August` + `Dummy Holidays` + `Dummy NewCar1 Full Launch` + 
             `Dummy OpenDoors Car1` + `Dummy OpenDoors Launch NewCar1` + `Dummy RunOut Car1`+
             `Investment TOT DirectCompetitor1+DirectCompetitor2`, data = data[-1,])
summary(m1.o)

# model 2 concave response
m2.o <- lm(`Orders Car1+NewCar1` ~ log(`GRP Radio`+1) + log(`GRP TV Car1`+1) + 
             log(`Other Offline Activities`+1) + log(`Investment ON Car1`+1) + log(`Investment Search Car1`+1) +
             log(`Investment Programmatic Car1`+1) + log(`Investment TOT UmbrellaBrand`+1) + 
             log(`Investment TOT UmbrellaBrand OtherModels`+1) + `Dummy August` +
             `Dummy Holidays` + `Dummy NewCar1 Full Launch` + 
             `Dummy OpenDoors Car1` + `Dummy OpenDoors Launch NewCar1` + `Dummy RunOut Car1`+
             `Investment TOT DirectCompetitor1+DirectCompetitor2`, data = data[-1,])
summary(m2.o)

# model 3 concave-quadratic response
m3.o <- lm(`Orders Car1+NewCar1` ~ `GRP Radio` + I(`GRP Radio`^2) + 
             `GRP TV Car1` + I(`GRP TV Car1`^2) +`Other Offline Activities` + 
             I(`Other Offline Activities`^2) + `Investment ON Car1` + I(`Investment ON Car1`^2) + 
             `Investment Search Car1` +I(`Investment Search Car1`^2) + `Investment Programmatic Car1`+
             I(`Investment Programmatic Car1`^2) + `Investment TOT UmbrellaBrand` + 
             I(`Investment TOT UmbrellaBrand`^2) + `Investment TOT UmbrellaBrand OtherModels` + 
             I(`Investment TOT UmbrellaBrand OtherModels`^2) + `Dummy August` + `Dummy Holidays` +
             `Dummy NewCar1 Full Launch` + `Dummy OpenDoors Car1` + `Dummy OpenDoors Launch NewCar1` + 
             `Dummy RunOut Car1` + `Investment TOT DirectCompetitor1+DirectCompetitor2`, data = data[-1,])

summary(m3.o)

# model 4 linear response with carryover
m4.o <- lm(`Orders Car1+NewCar1` ~ `GRP Radio` + `GRP TV Car1` + 
             `Other Offline Activities` + `Investment ON Car1` + `Investment Search Car1` +
             `Investment Programmatic Car1` + `Investment TOT UmbrellaBrand` + 
             `Investment TOT UmbrellaBrand OtherModels` +
             `Dummy August` + `Dummy Holidays` + `Dummy NewCar1 Full Launch` + 
             `Dummy OpenDoors Car1` + `Dummy OpenDoors Launch NewCar1` + `Dummy RunOut Car1` +
             lag.orders + `Investment TOT DirectCompetitor1+DirectCompetitor2`, data = data)
summary(m4.o)

# model 5 concave response with carryover
m5.o <- lm(`Orders Car1+NewCar1` ~ log(`GRP Radio`+1) + log(`GRP TV Car1`+1) + 
             log(`Other Offline Activities`+1) + log(`Investment ON Car1`+1) + log(`Investment Search Car1`+1) +
             log(`Investment Programmatic Car1`+1) + log(`Investment TOT UmbrellaBrand`+1) + 
             log(`Investment TOT UmbrellaBrand OtherModels`+1) +
             `Dummy August` + `Dummy Holidays` + `Dummy NewCar1 Full Launch` + 
             `Dummy OpenDoors Car1` + `Dummy OpenDoors Launch NewCar1` + `Dummy RunOut Car1`+
             lag.orders + `Investment TOT DirectCompetitor1+DirectCompetitor2`, data = data)
summary(m5.o)

# model 6 concave-quadratic response with carryover
m6.o <- lm(`Orders Car1+NewCar1` ~ `GRP Radio` + I(`GRP Radio`^2) + 
             `GRP TV Car1` + I(`GRP TV Car1`^2) +`Other Offline Activities` +
             I(`Other Offline Activities`^2) + `Investment ON Car1` + I(`Investment ON Car1`^2) + `Investment Search Car1` + 
             I(`Investment Search Car1`^2) +`Investment Programmatic Car1`+I(`Investment Programmatic Car1`^2) + 
             `Investment TOT UmbrellaBrand` + I(`Investment TOT UmbrellaBrand`^2) + `Investment TOT UmbrellaBrand OtherModels` + 
             I(`Investment TOT UmbrellaBrand OtherModels`^2) + `Dummy August` + `Dummy Holidays` + 
             `Dummy NewCar1 Full Launch` + `Dummy OpenDoors Car1` + `Dummy OpenDoors Launch NewCar1` + 
             `Dummy RunOut Car1`+ lag.orders + `Investment TOT DirectCompetitor1+DirectCompetitor2`, data = data)
summary(m6.o)

# best model: m5.o adj R^2 = 0.63
# inv (search), programmatic, Umbrella Others, Dummy August, Open Doors car1 + new car, (lag)


############# elasticity and marginal effects (quotations: using m4)

# lag (coefficient 16)
lambda <- as.numeric(m4$coefficients[16])

# GRP Radio (coefficient 2)
beta_radio <- as.numeric(m4$coefficients[2])

marg.effect.st.radio <- beta_radio * mean(data$`GRP Radio`) / mean(data$`Quotations Car1+NewCar1`)
marg.effect.lt.radio <- (beta_radio / (1 - lambda)) * mean(data$`GRP Radio`) / mean(data$`Quotations Car1+NewCar1`)

# GRP TV (coefficient 3)
beta_tv <- as.numeric(m4$coefficients[3])

marg.effect.st.tv <- beta_tv * mean(data$`GRP TV Car1`) / mean(data$`Quotations Car1+NewCar1`)
marg.effect.lt.tv <- (beta_tv / (1 - lambda)) * mean(data$`GRP TV Car1`) / mean(data$`Quotations Car1+NewCar1`)


# Investment Search (coefficient 6)
beta_search <- as.numeric(m4$coefficients[6])

marg.effect.st.search <- beta_search * mean(data$`Investment Search Car1`) / mean(data$`Quotations Car1+NewCar1`)
marg.effect.lt.search <- (beta_search / (1 - lambda)) * mean(data$`Investment Search Car1`) / mean(data$`Quotations Car1+NewCar1`)

# Investment Programmatic (coefficient 7)
beta_prog <- as.numeric(m4$coefficients[7])

marg.effect.st.prog <- beta_prog * mean(data$`Investment Programmatic Car1`) / mean(data$`Quotations Car1+NewCar1`)
marg.effect.lt.prog <- (beta_prog / (1 - lambda)) * mean(data$`Investment Programmatic Car1`) / mean(data$`Quotations Car1+NewCar1`)


# Investment UmbrellaBrand OtherModels (coefficient 9)
beta_umb_other <- as.numeric(m4$coefficients[9])

marg.effect.st.umb_other <- beta_umb_other * mean(data$`Investment TOT UmbrellaBrand OtherModels`) / mean(data$`Quotations Car1+NewCar1`)
marg.effect.lt.umb_other <- (beta_umb_other / (1 - lambda)) * mean(data$`Investment TOT UmbrellaBrand OtherModels`) / mean(data$`Quotations Car1+NewCar1`)


marg.effects <- data.frame(`Short Term` = c(marg.effect.st.radio, marg.effect.st.tv, marg.effect.st.search,
                             marg.effect.st.prog, marg.effect.st.umb_other), `Long Term` = c(marg.effect.lt.radio, 
                            marg.effect.lt.tv, marg.effect.lt.search, marg.effect.lt.prog, marg.effect.lt.umb_other))
row.names(marg.effects) <- c("GRP Radio", "GRP TV", "Investment Search", "Investment Programmatic", "Invetsment Umbrella Brand Other")

