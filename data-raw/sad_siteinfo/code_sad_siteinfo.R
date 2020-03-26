library(tidyverse)
library(readxl)

sad_siteinfo <- read_excel("data-raw/sad_siteinfo/rd_siteinfo-facts.xlsx") %>%
  mutate(site = str_sub(str_to_lower(site_name), 1, 4)) %>%
  filter(site != "kell") %>%
  rename(lon = long) %>%
  select(site_name, site, lat, lon, drainage, irrigation)

sad_siteinfo %>% write_csv("data-raw/sad_siteinfo/sad_siteinfo.csv")
use_data(sad_siteinfo, overwrite = TRUE)
