### libraries
library(dplyr)
library(readr)
library(data.table)
library(devtools)
devtools::install_github("ryanpeek/wateRshedTools")

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
fre.qc$Date <- as.Date(fre.qc$Date, "%m/%d/%Y")
str(fre.qc)
# explore quality codes
unique(fre.qc$Qual)
unique(fre.qc$...4)
sum(is.na(fre.qc$Point))#43437

fre.qc.na<- na.omit(fre.qc[,c(1,2)]) # just remove NA value for now

# this is hourly data, so need to calc max per day
Discharge.Sac <-
  fre.qc.na %>%
  group_by(Date) %>%
  summarise(Height.Sac = max(Point))

dayflow <- read_csv("data/dayflow-results-1997-2020.csv")
head(dayflow)
dayflow$Date <- as.Date(dayflow$Date)
str(dayflow)
plot(dayflow$Date, dayflow$YOLO)

All.flows <- merge(dayflow[,c(3,5)], Discharge.Sac, by = "Date")
head(All.flows)
min(All.flows$Date) #1996-10-01
max(All.flows$Date) #2020-09-30

plot(All.flows$Date, All.flows$Height.Sac)
plot(All.flows$Height.Sac, All.flows$YOLO)

# definition for inundation days

for(i in 1:nrow(All.flows)){
  if(All.flows[i,"Height.Sac"] < 33.5){
    All.flows[i,"Topped.days"] <- 0}
  else if(All.flows[i, "Height.Sac"] >= 33.5){
    All.flows[i, "Topped.days"] <- All.flows[i-1, "Topped.days"]+1}
    else {
    All.flows[i, "Topped.days"] <- 0 }
}

# jessica's addition to fix the tails
for(i in 2:nrow(All.flows)){
  if(All.flows[i, "YOLO"] >= 4000 & All.flows[i-1, "Topped.days"] > 0){
  All.flows[i, "Topped.days"] <- All.flows[i-1, "Topped.days"]+1}
}

max(All.flows$Topped.days) #91
All.flows.check <- subset(All.flows, Topped.days > 0)

head(All.flows)
write.csv(All.flows, "inundation_days.csv")

# add year
#All.flows[, "Year"] <- format(All.flows[,"Date"], "%Y")
unique(All.flows$WY)
# need to switch to water year
All.flows <- add_WYD(All.flows,"Date")

plot(All.flows$Date, All.flows$Topped.days)

# max inundation days per year
inundation.max<- aggregate(All.flows['Topped.days'], by=All.flows['WY'], max)
# need to add total inundation days per year
# flooding? yes (1), no (0)
All.flows$inundation <- ifelse(All.flows$Topped.days > 0, 1, 0)
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
overtopping$Year <- format(overtopping$Date, format = "%Y")
overtopping <- add_WYD(overtopping,"Date")
head(overtopping)

# first and last overtopping
topping.timing <- overtopping %>%
  group_by(WY) %>% #year is not working, need water year
  summarise(min = min(Date),
            max = max(Date))

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
overtopping$consecutive <- c(NA,diff(as.Date(overtopping$Date))==1)
num_events <-
  overtopping %>%
  filter(consecutive == FALSE) %>%
  group_by(WY) %>%
  summarise(number_of_events = n())

All.summarised <- merge(All.summarised, num_events, by = "WY", all.x = TRUE)
write.csv(All.summarised, "All.summarised.csv")

# look at daymet
# Yolo
lats <- 38.307857513
lons <- -121.692428589

library(daymetr)
yolo_daymet <- download_daymet(site = "Yolo",
                               lat = lats,
                               lon =  lons,
                               start = 1997,
                               end = 2020, internal = TRUE)


# rename variables, create a date column
yolo_daymet_df <- yolo_daymet$data %>%
  transmute(date = ymd(paste0(year, '01-01'))+ days(yday) -1,
            precip_mm = prcp..mm.day., # mm ppt / day
            tmax = tmax..deg.c., #max temp
            tmin = tmin..deg.c., # min temp
            tmean = (tmax + tmin) / 2, # mean temp
            trange = tmax - tmin, # temp range
            srad = srad..W.m.2., # soloar radiation
            vpd = vp..Pa.)
