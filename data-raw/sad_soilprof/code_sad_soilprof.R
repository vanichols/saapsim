library(tidyverse)
library(readxl)
library(janitor)


spaw <- read_csv("data-raw/sad_soiltext/td_sax-rawles-vals.csv") %>%
  mutate(site = str_to_lower(site),
         site = str_sub(site, 1, 4)) %>%
  filter(site != "kell",
         site != "musc")

#--keep just the values I care about

sad_soilprof <-
  spaw %>%
  clean_names() %>%
  select(-ll, -dul, -sat, -swcon, -ksat, -dp_mm) %>%
  rename(clay_pct = p_clay,
         sand_pct = p_sand,
         silt_pct = p_silt,
         soc_pct = p_soc,
         om_pct = p_om,
         bden_gmc3 = bden)


sad_soilprof %>% write_csv("data-raw/sad_soilprof/sad_soilprof.csv")
use_data(sad_soilprof, overwrite = TRUE)
