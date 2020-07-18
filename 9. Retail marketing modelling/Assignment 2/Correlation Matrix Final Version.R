library(tidyverse)
library(dplyr)
library(readxl)
library(tseries)
library(lubridate)
install.packages("minpack.lm")
library(minpack.lm)

# read data
data <- read_excel("DB_CarQuotes_Weekly_RMA.xlsx", sheet = 1)[-c(100:116),]
summary(data)

# decode date column
# data['Week'] <- as.Date(as.numeric(data$Week), origin = "1899-12-30")

# transform to numeric variables
cols = c("Lead Brochure Car1+NewCar1", "Lead Quotation Car1+NewCar1", "Lead TD Car1+NewCar1", 
         "Investment ON Car1", "Investment TV Car1 Copy1", "Investment TV Car1 Copy2", 
         "Investment TV Car1 Copy3", "Investment TV Car1 Copy4")    
data[,cols] = apply(data[,cols], 2, function(x) as.numeric(x))

# create time series for orders & quotations to extract seasonality + trend
# we use freq = 4 to detect any patterns from month to month (approx.)
ts_orders <- ts(data$`Orders Car1+NewCar1`, frequency = 4)
plot(stl(ts_orders,'periodic'))

ts_quotation <- ts(data$`Quotations Car1+NewCar1`, frequency = 4)
plot(stl(ts_quotation,'periodic'))

# unit root test 
# ADF says there is a unit root, the other two tests say no
# for Orders two of three test decline that there is unit root 
adf.test(data$`Orders Car1+NewCar1`)
pp.test(data$`Orders Car1+NewCar1`)
kpss.test(data$`Orders Car1+NewCar1`)

# for Quotations all three test decline that there is unit root 
adf.test(data$`Quotations Car1+NewCar1`)
pp.test(data$`Quotations Car1+NewCar1`)
kpss.test(data$`Quotations Car1+NewCar1`)


# introduce a lag variable for Quotations and Orders
data$lag.quotations <- c(NA, head(data$`Quotations Car1+NewCar1`, -1))
data$lag.orders <- c(NA, head(data$`Orders Car1+NewCar1`, -1))

# create new variable of total investment / Total GRP radio
data <- data %>% mutate("TOT Investment Radio" = `Investment Radio 1`+`Investment Radio 2`+`Investment Radio 3`,
                        "GRP Radio" = `GRP RADIO 1` + `GRP RADIO 2` + `GRP RADIO 3`)


# create a new variable "Other Offline Activities"
data <- data %>% mutate("Other Offline Activities" = `Investment OFF Car1` - `TOT Investment Radio` - `Investment TV Car1`)



################################################################################################################

### Correlation Matrix ###

install.packages('corrgram')
library(corrgram)


#selecting data from reelvant ativities and adding prices
s1 <- subset(data, select= c("Quotations Car1+NewCar1","GRP Radio","GRP TV Car1","Other Offline Activities",
                             "Investment ON Car1","Investment Search Car1",
                             "Investment TOT UmbrellaBrand OtherModels","Investment TOT DirectCompetitor1+DirectCompetitor2"))

corrgram(s1, order=NULL ,diag.panel= panel.density,lower.panel=panel.cor, upper.panel=panel.pts , text.panel=panel.txt)




