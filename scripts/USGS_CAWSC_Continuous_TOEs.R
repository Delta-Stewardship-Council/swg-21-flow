# retrieve USGS CAWSC continuous station data for Toe Drain (south of Stairstep)
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

siteNumbers <- c("11455140")

startDate <- ""

endDate <- ""

parameterCd <- c("00010", "00060", "00065", "00095", "72137", "72295", "63680")

#what instantaneous values (uv) are available

uvTOEs_Data <- whatNWISdata(siteNumbers = '11455140', service="uv") 

#pull instantaneous values - WARNING this will take a while

uvTOEs<- readNWISuv(siteNumbers, parameterCd, startDate, endDate)

head(uvTOEs)

write_csv(uvTOEs, "C:/Users/estumpne/Documents/R/NCEAS/uv_TOEs.csv")

# read in local file

uvTOEs <- read_csv("C:/Users/estumpne/Documents/R/NCEAS/uv_TOEs.csv")

head(uvTOEs)

#select columns

uvTOEs_select <-select(uvTOEs, site_no, dateTime, X_00010_00000, X_00060_00000, X_00065_00000, tz_cd)

#rename columns

uvTOEs_clean <-rename(uvTOEs_select, Temp = X_00010_00000, discharge = X_00060_00000, stage = X_00065_00000)

write_csv(uvTOEs_clean, "C:/Users/estumpne/Documents/R/NCEAS/uv_TOEs_clean.csv")
  
#produce daily summary  

dailyTOEs <- uvTOEs %>%
  mutate(date = as.Date(dateTime)) %>%
  group_by(date) %>%
  summarize(mean_Temp = mean(X_00010_00000, NA.rm=TRUE), 
            mean_discharge = mean(X_00060_00000, NA.rm=TRUE), 
            mean_stage = mean(X_00065_00000, NA.rm=TRUE))


write_csv(dailyTOEs, "C:/Users/estumpne/Documents/R/NCEAS/daily_TOEs.csv")

#use floor_date function for to downstep 15-min to hourly mean

hourlyTOEs <- uvTOEs %>%
  #mutate(date = hour(dateTime)) %>%
  
  group_by(dateTime=floor_date(dateTime, "1 hour")) %>%
  summarize(mean_Temp = mean(X_00010_00000, NA.rm=TRUE), 
            mean_discharge = mean(X_00060_00000, NA.rm=TRUE), 
            mean_stage = mean(X_00065_00000, NA.rm=TRUE))

write_csv(hourlyTOEs, "C:/Users/estumpne/Documents/R/NCEAS/hourly_TOEs.csv")

#use floor_date function to downstep 15-min to monthly mean

monthlyTOEs <- uvTOEs %>%
  #mutate(date = hour(dateTime)) %>%
  group_by(dateTime=floor_date(dateTime, "month")) %>%
  summarize(mean_Temp = mean(X_00010_00000, NA.rm=TRUE), 
            mean_discharge = mean(X_00060_00000, NA.rm=TRUE), 
            mean_stage = mean(X_00065_00000, NA.rm=TRUE))

write_csv(monthlyTOEs, "C:/Users/estumpne/Documents/R/NCEAS/monthly_TOEs.csv")

