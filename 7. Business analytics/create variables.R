# import variables
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
setwd("~/00. Spring/Business Analytics/Group Project/Dataset")
stop_and_search <- read_csv("2016-07-metropolitan-stop-and-search.csv")
crimes <- read_csv("2016-07-metropolitan-crimes.csv")
events_by_postcode <- read_csv("events by postcode.csv")

# describe: create "month" for select July data
stop_and_search <- stop_and_search %>% mutate(month=month(ymd_hms(stop_and_search$Date)))
stop_and_search <- stop_and_search %>% filter(month==7)

# evaluation: create "SSO" for searching outcome
stop_and_search <- stop_and_search %>% 
  mutate(SSO = ifelse(grepl('Nothing', stop_and_search$Outcome), 'Nothing', 'Something'),
         Something = ifelse(SSO =='Something', 1, 0))

# Explain: create "hour" and "time_of_day" for regression
stop_and_search <- stop_and_search %>% mutate(hour=hour(ymd_hms(stop_and_search$Date)),
                                              time_of_day=case_when(hour %in% c(5,6,7,8,9)~"morning",
                                                                    hour %in% c(10,11,12,13,14,15,16)~"noon",
                                                                    hour %in% c(17,18,19,20,21,22)~"evening",
                                                                    hour %in% c(23,0,1,2,3,4)~"midnight")) %>% select(-hour)
  
# Predict
ggplot(events_by_postcode, aes(x=something, y=Total, col=ratio))+geom_point()

