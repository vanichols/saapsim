library(tidyverse)
library(readxl)
library(janitor)



swt <- read_excel("data-raw/sad_soilchar/archontoulis-et-al-2020-supplementary-table-S4.xlsx") %>%
  select(-location) %>%
  mutate(site = str_to_lower(site),
         site = str_sub(site, 1, 4)) %>%
  filter(site != "kell",
         site != "musc",
         !is.na(site))

sad_soilchar <- swt


sad_soilchar %>% write_csv("data-raw/sad_soilchar/sad_soilchar.csv")
use_data(sad_soilchar, overwrite = TRUE)
