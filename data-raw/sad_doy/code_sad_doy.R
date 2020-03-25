library(tidyverse)
library(readxl)
library(lubridate)

#note: suth 2001 and 2002 didn't have their dates recorded. I just took an average to fill it in

#--yield gaps at max N
sad_plant1 <-
  read_excel("data-raw/sad_plant/20200325_Mitch-planting-dates.xlsx") %>%
  mutate(plant_month = month(planting_date),
         plant_mday = mday(planting_date),
         plant_date = as_date(paste(year,
                                    plant_month,
                                    plant_mday,
                                    sep = "-")),
         plant_doy = yday(plant_date),
         site = tolower(site),
         site = str_sub(site, 1, 4)) %>%
    select(-planting_date, -plant_month, -plant_mday)

#--address suth 2001/2002

suth_avg <-
  sad_plant1 %>%
  filter(site == "suth") %>%
  summarise(plant_doy_mean = mean(plant_doy, na.rm = T)) %>%
  pull() %>%
  round(0)

sad_plant1 %>%
  mutate(plant_doy = ifelse( (site == "sutherland" & is.na(plant_doy)),
                             suth_avg,
                             plant_doy),
         plant_date = ifelse( (site == "sutherland" & is.na(plant_doy)),
                              suth_avg,
                              plant_doy),)

sad_plant
use_data(sad_cgap)
