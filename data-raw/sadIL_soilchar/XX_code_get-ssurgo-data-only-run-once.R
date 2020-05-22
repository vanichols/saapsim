# Created:       3/25/2020
# last edited:   3/26/2020 (upgrade from practice to actual lat/lon)
#                5/22/2020 (applied to IL data)
#
# purpose: use feddata package to get ssurgo data
# notes: only do this once to download data


rm(list = ls())
library(saapsim) #--sot-related pkg
library(tidyverse)
library(janitor)
library(lubridate)
library(FedData)
library(sf)
library(tidysawyer2)


# lat/lon each site (from saapsim pkg) ------------------------------------

sll <-
  il_siteinfo %>%
  mutate(site = str_sub(str_to_lower(site_name), 1, 4),
         site = case_when(
           grepl("Orr Center", site_name) ~ "orrc",
           grepl("Dixon Springs Upland", site_name) ~ "dixu",
           grepl("Dixon Springs Bottomland", site_name) ~ "dixb",
         TRUE ~ site)
         ) %>%
  #--keep this
  select(site, lat, lon)


# start data download (only do once) --------------------------------------
mysites <- sll %>% select(site) %>% pull()

mychoice <- mysites[8]
mychoice
# dixb, 11:05
# dixu, 11:04am
# brow, 11am
# deka, 10:28am
# monm, 10:35am
# urba, 10:49am
# orrc, 10:56am

dat <-
  sll %>%
  filter(site == mychoice)


#--get lat/long for site
lonlat <- c(dat$lon, dat$lat)

shft <- 1e-3 ## This is 111 meters
lonlat.mat <- rbind(lonlat, ##root
                    lonlat + c(shft,0), ## x = 1, y = 0
                    lonlat + c(shft,shft), ## x = 1, y = 1
                    lonlat + c(0, shft), ## x = 0, y = 1
                    lonlat) ## back to root
rownames(lonlat.mat) <- NULL

#--the previous matrix is a rectangle, create a spatial polygon
pg <- Polygon(lonlat.mat)
spg <- SpatialPolygons(list(Polygons(list(pg), "s1")),
                       proj4string = CRS("+proj=longlat +datum=WGS84"))

#--does it look normal?
ggplot() + geom_sf(data = st_as_sf(spg))


# use get_ssurgo ----------------------------------------------------------

# only necessary once!
# label should be what to call that profile
# notice that our template is the 'SpatialPolygon' we created
# NOTE: This step may take awhile (minutes?)

soil <- get_ssurgo(template = spg, label = mychoice)


