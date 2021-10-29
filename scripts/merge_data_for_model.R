# stitch inundation with verona river flow

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)


# Bring in Inundation Data ------------------------------------------------

# this has DayFlow Yolo, Stage of Sac at Fremont Weir, inundation days (from Pascale's code inundation_days.R)

inund <- read_csv("data/inundation_days.csv") %>%
  select(Date:Topped.days) # drop row id column

# Get Verona Daily Flow ---------------------------------------------------

library(dataRetrieval)

# discharge is "00060"
verona <- dataRetrieval::readNWISdv(siteNumbers = "11425500", parameterCd = c("00060"))

# fix names
verona <- addWaterYear(verona)
verona <- dataRetrieval::renameNWISColumns(verona)

# write out
write_csv(verona, "data/usgs_verona_discharge_1929-2021.csv")

# quick plot
(g1 <-ggplot(verona) + geom_line(aes(x=Date, y=Flow)) +
  geom_line(data=inund, aes(x=Date, y=YOLO), color="blue"))

# use plotly to interactively visualize
library(plotly)
ggplotly(g1)


# Merge Data --------------------------------------------------------------

# only merge for same period of record of inund data

df_out <- left_join(inund, verona, by="Date") %>%
  # drop columns
  select(-c(agency_cd, Flow_cd)) %>%
  # rename
  rename(Flow_usgs_verona = Flow,
         site_no_usgs = site_no)

summary(df_out)


# write out
write_csv(df_out, "data/yolo_flow_inund_fremont.csv")
