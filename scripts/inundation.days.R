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
unique(All.flows$Year)
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
  group_by(Year) %>%
  filter(YOLO == max(YOLO)) %>%
  distinct(YOLO,.keep_all = T) %>%
  ungroup()

write.csv(flood.timing, "flood_peak_by_year.csv")

# FRE overtopping events
overtopping <- subset(Discharge.Sac, Height.Sac >= 33.5)
overtopping$Year <- format(overtopping$Date, format = "%Y")

# first and last overtopping
topping.timing <- overtopping %>%
  group_by(Year) %>% #year is not working, need water year
  summarise(min = min(Date),
            max = max(Date))

overtopping %>%
  # find min and max
  summarise(min = min(Date),
            max = max(Date))
# number of days of overtopping
ddply(DF, .(months), summarise,
      reached =
        if (any(cumsum(Rainfall)>= thresh)) {
          which.max(
            cumsum(Rainfall) >= thresh)
        } else NA)
