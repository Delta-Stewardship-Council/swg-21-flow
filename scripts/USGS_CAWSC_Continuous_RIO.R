# retrieve USGS CAWSC continuous station data for Rio Vista Bridge
#downstep 15 minute data 1 hour, daily, monthly data
#limited to discharge, tidally filtered discharge, gage height, and velocity but can be used for other parameters
#read in parameter code ("pcode") file
# created 10/12/21 by Elizabeth B. Stumpner

getwd()

setwd("C:/Users/estumpne/Documents/R/NCEAS")

library(dataRetrieval)
library(readr)
library(lubridate)
library(dplyr)

siteNumbers <- c("11455420")

startDate <- ""

endDate <- ""

parameterCd <- c("00010", "00060", "00065", "00095", "72137", "72295", "63680")

#what instantaneous values (uv) are available

uvData <- whatNWISdata(siteNumbers = '11455420', service="uv") 

#pull instantaneous values - WARNING this will take a while

uvRIO <- readNWISuv(siteNumbers, parameterCd, startDate, endDate)

head(uvRIO)

write_csv(uvRIO, "C:/Users/estumpne/Documents/R/NCEAS/uv_RIO.csv")

# read in local file

uvRIO <- read_csv("C:/Users/estumpne/Documents/R/NCEAS/uv_RIO.csv")

head(uvRIO)

#select columns

uvRIO_select <-select(uvRIO, site_no, dateTime, X_00010_00000, X_00060_00000, X_00065_00000, tz_cd)

#rename columns

uvRIO_clean <-rename(uvRIO_select, Temp = X_00010_00000, discharge = X_00060_00000, stage = X_00065_00000)

write_csv(uvRIO_clean, "C:/Users/estumpne/Documents/R/NCEAS/uv_RIO_clean.csv")
  
#produce daily summary  

dailyRIO <- uvRIO %>%
  mutate(date = as.Date(dateTime)) %>%
  group_by(date) %>%
  summarize(mean_Temp = mean(X_00010_00000, NA.rm=TRUE), 
            mean_discharge = mean(X_00060_00000, NA.rm=TRUE), 
            mean_stage = mean(X_00065_00000, NA.rm=TRUE))


write_csv(dailyRIO, "C:/Users/estumpne/Documents/R/NCEAS/daily_RIO.csv")

#use floor_date function for to downstep 15-min to hourly mean

hourlyRIO <- uvRIO %>%
  #mutate(date = hour(dateTime)) %>%
  
  group_by(dateTime=floor_date(dateTime, "1 hour")) %>%
  summarize(mean_Temp = mean(X_00010_00000, NA.rm=TRUE), 
            mean_discharge = mean(X_00060_00000, NA.rm=TRUE), 
            mean_stage = mean(X_00065_00000, NA.rm=TRUE))

write_csv(hourlyRIO, "C:/Users/estumpne/Documents/R/NCEAS/hourly_RIO.csv")

#use floor_date function to downstep 15-min to monthly mean

monthlyRIO <- uvRIO %>%
  #mutate(date = hour(dateTime)) %>%
  group_by(dateTime=floor_date(dateTime, "month")) %>%
  summarize(mean_Temp = mean(X_00010_00000, NA.rm=TRUE), 
            mean_discharge = mean(X_00060_00000, NA.rm=TRUE), 
            mean_stage = mean(X_00065_00000, NA.rm=TRUE))

write_csv(monthlyRIO, "C:/Users/estumpne/Documents/R/NCEAS/monthly_RIO.csv")

