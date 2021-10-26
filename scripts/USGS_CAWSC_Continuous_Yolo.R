# retrieve USGS CAWSC continuous station data for Yolo at Woodland
#downstep 15 minute data 1 hour, daily, monthly data
#limited to discharge and gage height but can be used for other parameters
#see parameter code file ("pcode.csv") 
# created 10/19/21 by Elizabeth B. Stumpner

getwd()

setwd("C:/Users/estumpne/Documents/R/NCEAS")

library(dataRetrieval)
library(readr)
library(lubridate)
library(dplyr)

siteNumbers <- c("11453000")

startDate <- ""

endDate <- ""

parameterCd <- c("00010", "00060", "00065", "00095", "72137", "72295", "63680")

#what instantaneous values (uv) are available

whatuvData_YOLO <- whatNWISdata(siteNumbers = '11453000', service="uv") 

#pull instantaneous values - WARNING this will take a while 

uvYOLO<- readNWISuv(siteNumbers, parameterCd, startDate, endDate)

head(uvYOLO)

write_csv(uvYOLO, "C:/Users/estumpne/Documents/R/NCEAS/uv_YOLO.csv")

# read in local file

uvYOLO <- read_csv("C:/Users/estumpne/Documents/R/NCEAS/uv_YOLO.csv")

head(uvCCH)

#select columns

uvYOLO_select <-select(uvYOLO, site_no, dateTime, X_00010_00000, X_00060_00000, X_00065_00000, tz_cd)

#rename columns

uvYOLO_clean <-rename(uvYOLO_select, Temp = X_00010_00000, discharge = X_00060_00000, stage = X_00065_00000)
                                        
write_csv(uvYOLO_clean, "C:/Users/estumpne/Documents/R/NCEAS/uv_YOLO_clean.csv")

#produce daily summary  

dailyYOLO <- uvYOLO %>%
  mutate(date = as.Date(dateTime)) %>%
  group_by(date) %>%
  summarize(mean_Temp = mean(X_00010_00000, NA.rm=TRUE), 
            mean_discharge = mean(X_00060_00000, NA.rm=TRUE), 
            mean_stage = mean(X_00065_00000, NA.rm=TRUE))


write_csv(dailyYOLO, "C:/Users/estumpne/Documents/R/NCEAS/daily_YOLO.csv")

#use floor_date function for to downstep 15-min to hourly mean

hourlyYOLO <- uvYOLO %>%
  #mutate(date = hour(dateTime)) %>%
  
  group_by(dateTime=floor_date(dateTime, "1 hour")) %>%
  summarize(mean_Temp = mean(X_00010_00000, NA.rm=TRUE), 
             mean_discharge = mean(X_00060_00000, NA.rm=TRUE), 
             mean_stage = mean(X_00065_00000, NA.rm=TRUE))

write_csv(hourlyYOLO, "C:/Users/estumpne/Documents/R/NCEAS/hourly_YOLO.csv")

#use floor_date function to downstep 15-min to monthly mean

monthlyYOLO <- uvYOLO %>%
  #mutate(date = hour(dateTime)) %>%
  group_by(dateTime=floor_date(dateTime, "month")) %>%
  summarize(mean_Temp = mean(X_00010_00000, NA.rm=TRUE), 
            mean_discharge = mean(X_00060_00000, NA.rm=TRUE), 
            mean_stage = mean(X_00065_00000, NA.rm=TRUE))

write_csv(monthlyYOLO, "C:/Users/estumpne/Documents/R/NCEAS/monthly_YOLO.csv")


