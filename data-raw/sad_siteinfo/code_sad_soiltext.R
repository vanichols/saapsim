library(tidyverse)
library(readxl)
library(janitor)


spaw <- read_csv("data-raw/sad_soiltext/td_sax-rawles-vals.csv") %>%
  mutate(site = str_to_lower(site),
         site = str_sub(site, 1, 4))


#--keep just the values I care about

sad_soiltext <-
  spaw %>%
  clean_names() %>%
  select(-ll, -dul, -sat, -swcon, -ksat, -dp_mm) %>%
  rename(clay_pct = p_clay,
         sand_pct = p_sand,
         silt_pct = p_silt,
         soc_pct = p_soc,
         om_pct = p_om,
         bden_gmc3 = bden)


sad_soiltext %>% write_csv("data-raw/sad_soiltext/sad_soiltext.csv")
use_data(sad_soiltext, overwrite = TRUE)
