### libraries
library(dplyr)
library(readr)
library(data.table)
library(lubridate)
library(devtools)
#devtools::install_github("ryanpeek/wateRshedTools")
library(wateRshedTools)

### flow data
#sac <- read.csv("data/FRE_1.csv")
#sac$Date <- as.Date(sac$Date)
#str(sac)
#plot(sac$Date, sac$VALUE)

# review data flags
#subset(sac, DATA_FLAG == "N") # weird....

# this FRE data has QC
fre.qc <- read_csv("data/WDL_FRE_A02170_Stage_Raw.zip", skip = 2)
head(fre.qc)

# fix datetimes and dates
fre.qc$Datetime <- mdy_hms(fre.qc$Date)
fre.qc$date <- as.Date(fre.qc$Datetime)
summary(fre.qc)
str(fre.qc)
# explore quality codes
unique(fre.qc$Qual)
table(fre.qc$Qual)
unique(fre.qc$...4)
sum(is.na(fre.qc$Point))#43437

# this drops the days we may need later.
# fre.qc.na<- na.omit(fre.qc[,c(1,2)]) # just remove NA value for now

# this is hourly data, so need to calc max per day
Discharge.Sac <-
  fre.qc %>%
  group_by(date) %>%
  summarise(Height.Sac = max(Point, na.rm = TRUE)) %>%
  ungroup()

# to deal with no/Inf data, for now just inserting stage of 10, resulting in zero flow days later, almost all dates from late summer/fall and at low flows...
Discharge.Sac <- Discharge.Sac %>%
  mutate(Height.Sac = ifelse(is.infinite(Height.Sac), 10, Height.Sac),
         Height.Sac = ifelse(is.na(Height.Sac), 10, Height.Sac))
summary(Discharge.Sac)

dayflow <- read_csv("data/dayflow-results-1997-2020.csv")
head(dayflow)
dayflow$Date <- ymd(dayflow$Date)
summary(dayflow)
plot(dayflow$Date, dayflow$YOLO)

All.flows <- left_join(dayflow[,c(3,5)], Discharge.Sac, by =c("Date"="date"))

# as before, make NAs 10 for now
All.flows <- All.flows %>%
  mutate(Height.Sac = ifelse(is.na(Height.Sac), 10, Height.Sac))

summary(All.flows)
min(All.flows$Date) #1996-10-01
max(All.flows$Date) #2020-09-30

plot(All.flows$Date, All.flows$Height.Sac)
plot(All.flows$Height.Sac, All.flows$YOLO)

# definition for inundation days
for(i in 1:nrow(All.flows)){
  if(All.flows[i,"Height.Sac"] < 33.5){
    All.flows[i,"Inund.days"] <- 0}
  else if(All.flows[i, "Height.Sac"] >= 33.5){
    All.flows[i, "Inund.days"] <- All.flows[i-1, "Inund.days"]+1}
    else {
    All.flows[i, "Inund.days"] <- 0 }
}

# jessica's addition to fix the tails
for(i in 2:nrow(All.flows)){
  if(All.flows[i, "YOLO"] >= 4000 & All.flows[i-1, "Inund.days"] > 0){
  All.flows[i, "Inund.days"] <- All.flows[i-1, "Inund.days"]+1}
}

max(All.flows$Inund.days) #91
All.flows.check <- subset(All.flows, Inund.days > 0)

head(All.flows)

# need to add total inundation days per year
# flooding? yes (1), no (0)
All.flows$inundation <- ifelse(All.flows$Inund.days > 0, 1, 0)
head(All.flows)

# 311 records with Stage of 10 are the "filled" records
# fix names
library(janitor)
all_flows <- All.flows %>% clean_names()

write.csv(all_flows, "data/inundation_days.csv", row.names = FALSE)

# add year
#All.flows[, "Year"] <- format(All.flows[,"Date"], "%Y")
# need to switch to water year
All.flows <- add_WYD(All.flows,"Date")
unique(All.flows$WY)
plot(All.flows$Date, All.flows$Inund.days)

# max inundation days per year
inundation.max<- aggregate(All.flows['Inund.days'], by=All.flows['WY'], max)

# if value, then 1, use sum with code above
inundation.total<- aggregate(All.flows['inundation'], by=All.flows['WY'], sum)

# max of QYOLO (flood peak) by year
#flood.peak<- aggregate(All.flows['YOLO'], by=All.flows['Year'], max)

# date of and flood peak value (max of QYOLO) by year
flood.timing <- All.flows %>%
  group_by(WY) %>%
  filter(YOLO == max(YOLO)) %>%
  distinct(YOLO,.keep_all = T) %>%
  ungroup()

write.csv(flood.timing, "flood_peak_by_year.csv")

# FRE overtopping events
overtopping <- subset(Discharge.Sac, Height.Sac >= 33.5)
overtopping$Year <- format(overtopping$date, format = "%Y")
overtopping <- add_WYD(overtopping,"date")
head(overtopping)

# first and last overtopping
topping.timing <- overtopping %>%
  group_by(WY) %>% #year is not working, need water year
  summarise(min = min(date),
            max = max(date))

topping_summarised <-
  All.flows %>%
  filter(Height.Sac > 33.5) %>%
  group_by(WY) %>%
  summarise(number_of_days = n(), # count number of rows in each group
            first_date = min(Date),
            last_date = max(Date))

# need to add last day of flooding
inundation_summarised <-
  All.flows %>%
  filter(inundation > 0) %>%
  group_by(WY) %>%
  summarise(number_of_days = n(), # count number of rows in each group
            first_date = min(Date),
            last_date = max(Date))

# put the summaries together
# make column names more translatable
head(inundation_summarised)
colnames(inundation_summarised) <- c("WY", "number_of_days_inund", "first_date_inund", "last_date_inund")
head(flood.timing)
colnames(flood.timing)[1:4] <- c("Date_peak", "YOLO_peak", "Height.Sac_peak", "Topped.days_peak")

All.summarised <- merge(inundation_summarised, flood.timing, by = "WY")
All.summarised <- All.summarised[,c(1:8)]

# number of overtopping events
#All.flows$over_Sac <- ifelse(All.flows$Height.Sac > 33.5, 1, 0)
# find gaps in dates
head(overtopping)
#All.flows[!All.flows$Date %in% overtopping$Date]
# consecutive dates T/F
overtopping$consecutive <- c(NA,diff(as.Date(overtopping$date))==1)
num_events <-
  overtopping %>%
  filter(consecutive == FALSE) %>%
  group_by(WY) %>%
  summarise(number_of_events = n())

All.summarised <- merge(All.summarised, num_events, by = "WY", all.x = TRUE)
write.csv(All.summarised, "data/All_summarised.csv")

# plots
plot(All.flows$Date, All.flows$Inund.days, col = "red")
par(new = TRUE)
plot(All.flows$Date, All.flows$YOLO, col = "blue")
par(new = TRUE)
plot(All.flows$Date, All.flows$Height.Sac, col = "green")

#install_github('htmlwidgets/sparkline')
library(htmlwidgets)
library(sparkline)


sparkline(All.flows[,2], type = 'bar')
lapply(All.flows[,c(2:4)], sparkline, type = 'bar')

