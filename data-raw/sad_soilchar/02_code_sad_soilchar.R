library(tidyverse)
library(usethis)
library(readxl)
library(janitor)


#--water table depth from FACTS paper
swt <- read_excel("data-raw/sad_soilchar/archontoulis-et-al-2020-supplementary-table-S4.xlsx") %>%
  select(-location) %>%
  mutate(site = str_to_lower(site),
         site = str_sub(site, 1, 4)) %>%
  filter(site != "kell",
         site != "musc",
         !is.na(site))

#--iacsr, b horizon depth, and drainage class from ssurgo
srgo <- read_csv("data-raw/sad_soilchar/td_ssurgo-vals.csv")



sad_soilchar <- swt %>%
  left_join(srgo) %>%
  select(site, everything())

sad_soilchar %>% write_csv("data-raw/sad_soilchar/sad_soilchar.csv")
use_data(sad_soilchar, overwrite = TRUE)
