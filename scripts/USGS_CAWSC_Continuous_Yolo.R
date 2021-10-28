# retrieve USGS CAWSC continuous station data for Yolo at Woodland
#downstep 15 minute data 1 hour, daily, monthly data
#limited to discharge and gage height but can be used for other parameters
#see parameter code file ("pcode.csv")
# created 10/19/21 by Elizabeth B. Stumpner

library(dataRetrieval)
library(readr)
library(lubridate)
library(dplyr)
library(here)

siteNumbers <- c("11453000")
startDate <- ""
endDate <- ""
parameterCd <- c("00010", "00060", "00065", "00095", "72137", "72295", "63680")

#what instantaneous values (uv) are available

whatuvData_YOLO <- whatNWISdata(siteNumbers = '11453000', service="uv")
whatuvData_YOLO
#pull instantaneous values - WARNING this will take a while

uvYOLO<- readNWISuv(siteNumbers, parameterCd, startDate, endDate)

head(uvYOLO)

write_csv(uvYOLO, "data/uv_YOLO.csv.zip")

# read in local file
uvYOLO <- read_csv("data/uv_YOLO_clean.csv.zip")

# rename with data retrieval colnames
uvYOLO_clean <- renameNWISColumns(uvYOLO)
uvYOLO_clean <- addWaterYear(uvYOLO_clean)

# drop columns that we don't need:
uvYOLO_clean <- select(uvYOLO_clean, agency_cd:Flow_Inst_cd, tz_cd)

# write out
write_csv(uvYOLO_clean, "data/uv_YOLO_clean.csv.zip")

#produce daily summary
dailyYOLO <- uvYOLO_clean %>%
  mutate(date = as.Date(dateTime)) %>%
  group_by(date) %>%
  summarize(mean_Temp = mean(Wtemp_Inst, NA.rm=TRUE),
            mean_discharge = mean(Flow_Inst, NA.rm=TRUE))

write_csv(dailyYOLO, "data/daily_YOLO.csv")

#use floor_date function for to downstep 15-min to hourly mean
hourlyYOLO <- uvYOLO_clean %>%
  group_by(dateTime=floor_date(dateTime, "1 hour")) %>%
  summarize(mean_Temp = mean(Wtemp_Inst, NA.rm=TRUE),
            mean_discharge = mean(Flow_Inst, NA.rm=TRUE))

write_csv(hourlyYOLO, "data/hourly_YOLO.csv")

#use floor_date function to downstep 15-min to monthly mean
monthlyYOLO <- uvYOLO_clean %>%
  group_by(dateTime=floor_date(dateTime, "month")) %>%
  summarize(mean_Temp = mean(Wtemp_Inst, NA.rm=TRUE),
            mean_discharge = mean(Flow_Inst, NA.rm=TRUE))

write_csv(monthlyYOLO, "data/monthly_YOLO.csv")


