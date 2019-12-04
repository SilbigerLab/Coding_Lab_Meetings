#OISST_data
#Adapted by Amanda Chiachi
#11/13/2019

#### Setup ##########
# The two packages we will need
# NB: The packages only need to be installed from GitHub once
#if.require("devtools")<-install.packages("devtools")
library(devtools)
#devtools::install_github("tidyverse/tidyverse")
#devtools::install_github("ropensci/rerddap")

# Load the packages once they have been downloaded and installed
library(tidyverse)
library(rerddap)
#if.require("heatwaveR")<-install.packages("heatwaveR")
library(heatwaveR)

# The information for the NOAA OISST data
rerddap::info(datasetid = "ncdc_oisst_v2_avhrr_by_time_zlev_lat_lon", 
              url = "https://www.ncei.noaa.gov/erddap/")

# This function expects the user to provide it with a start and end date
# It then downloads and prepares the data
# we are downloading only the SST data out of several variables 
# spatial extent of latitude 36.4, 37.1 & longitude 237.66, 238.23
OISST_sub <- function(time_df){
  oisst_res <- griddap(x = "ncdc_oisst_v2_avhrr_by_time_zlev_lat_lon", 
                       url = "https://www.ncei.noaa.gov/erddap/", 
                       time = c(time_df$start, time_df$end), 
                       depth = c(0, 0),
                       latitude = c(36.4, 37.1),
                       longitude = c(237.66, 238.23),
                       fields = "sst")$data %>% 
    mutate(time = as.Date(str_remove(time, "T00:00:00Z"))) %>% 
    dplyr::rename(t = time, temp = sst) %>% 
    select(lon, lat, t, temp) %>% 
    na.omit()}

#server doesnt like more than 9 years of consecutive data 
#creates a data frame to download multiple batches of the data 
# allows us to automate the entire download 
# Date download range by start and end dates per year
monterey_years <- data.frame(date_index = 1,
                             start = as.Date(c("2015-01-01")),
                             end = as.Date(c("2019-08-29")))
OISST_sub(monterey_years)

# Download all of the data with one nested request
# The time this takes will vary greatly based on connection speed
system.time(
  OISST_data <- monterey_years %>% 
    group_by(date_index) %>% 
    group_modify(~OISST_sub(.x)) %>% 
    ungroup() %>% 
    select(lon, lat, t, temp)) 
# user  system elapsed 
# 0.670   0.018   1.353
# Save the data as an .Rda file as it has a much better compression rate than .RData
saveRDS(OISST_data, file="data/Monterey.Rproj.Rda")

#dates saved as ""2015-01-01", end_date = "2019-08-29"
mydates<-c("2015-01-01", "2019-08-29")

# now move to the next set of code!

##################Visualizing Marine Heatwaves ##################

head(OISST_data)
#pulling out date and temp, do not need lat and lon
Monterey_data<-subset(OISST_data[,3:4])
head(Monterey_data)
tail(Monterey_data)

# is daily average above or below our threshold 
# Load libraries
# library(tidyverse)
# library (heatwaveR)
# should already have these 

## can look at average temperature by day
Monterey_data1<-Monterey_data %>%
  group_by(t) %>%
  summarise(temp = mean(temp))

# Detect the events in a time series
# "...must be at least three years to calculate thresholds"
# create your time series data 
# ts <- ts2clm(Monterey_data1, climatologyPeriod = mydates)
ts <- ts2clm(Monterey_data1, climatologyPeriod = c("2015-01-01", "2019-08-29"))

mhw <- detect_event(ts)

# View just a few metrics
mhw$event %>% 
  dplyr::ungroup() %>%
  dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative) %>% 
  dplyr::arrange(-intensity_max) %>% 
  head(5)

# this is really cool! it shows event duration, date start and peak, intensity max and cumulative intensity


#####Visualizing Marine Heatwaves######
event_line(mhw, spread = 550, metric = "intensity_max", 
           start_date = "2018-01-01", end_date = "2019-08-29") #category = TRUE for categories
lolli_plot(mhw, metric = "intensity_max")

#end 