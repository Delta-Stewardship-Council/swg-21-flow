# retrieve USGS CAWSC continuous station data for Cache Slough at Ryer Island
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

siteNumbers <- c("11455350")

startDate <- ""

endDate <- ""

parameterCd <- c("00010", "00060", "00065", "00095", "72137", "72295", "63680")

#what instantaneous values (uv) are available

uvData <- whatNWISdata(siteNumbers = '11455350', service="uv") 

#pull instantaneous values - WARNING this will take a while

uvCCH <- readNWISuv(siteNumbers, parameterCd, startDate, endDate)

head(uvCCH)

write_csv(uvCCH, "C:/Users/estumpne/Documents/R/NCEAS/uv_CCH.csv")

# read in local file

uvCCH <- read_csv("C:/Users/estumpne/Documents/R/NCEAS/uv_CCH.csv")

head(uvCCH)

#select columns

uvCCH_select <-select(uvCCH, site_no, dateTime, X_00010_00000, X_00060_00000, X_00065_00000, tz_cd)

#rename columns

uvCCH_clean <-rename(uvCCH_select, Temp = X_00010_00000, discharge = X_00060_00000, stage = X_00065_00000)

write_csv(uvCCH_clean, "C:/Users/estumpne/Documents/R/NCEAS/uv_CCH_clean.csv")

#produce daily summary  

dailyCCH <- uvCCH %>%
  mutate(date = as.Date(dateTime)) %>%
  group_by(date) %>%
  summarize(mean_Temp = mean(X_00010_00000, NA.rm=TRUE), 
            mean_discharge = mean(X_00060_00000, NA.rm=TRUE), 
            mean_stage = mean(X_00065_00000, NA.rm=TRUE))


write_csv(dailyCCH, "C:/Users/estumpne/Documents/R/NCEAS/daily_CCH.csv")

#use floor_date function for to downstep 15-min to hourly mean

hourlyCCH <- uvCCH %>%
  #mutate(date = hour(dateTime)) %>%
  
  group_by(dateTime=floor_date(dateTime, "1 hour")) %>%
  summarize(mean_Temp = mean(X_00010_00000, NA.rm=TRUE), 
            mean_discharge = mean(X_00060_00000, NA.rm=TRUE), 
            mean_stage = mean(X_00065_00000, NA.rm=TRUE))

write_csv(hourlyCCH, "C:/Users/estumpne/Documents/R/NCEAS/hourly_CCH.csv")

#use floor_date function to downstep 15-min to monthly mean

monthlyCCH <- uvCCH %>%
  #mutate(date = hour(dateTime)) %>%
  group_by(dateTime=floor_date(dateTime, "month")) %>%
  summarize(mean_Temp = mean(X_00010_00000, NA.rm=TRUE), 
            mean_discharge = mean(X_00060_00000, NA.rm=TRUE), 
            mean_stage = mean(X_00065_00000, NA.rm=TRUE))

write_csv(monthlyCCH, "C:/Users/estumpne/Documents/R/NCEAS/monthly_CCH.csv")


