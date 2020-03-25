library(tidyverse)
library(saapsim)

#--yield gaps at max N
sad_cgap <-
  sad_tidysawyer %>%
  group_by(crop, site, rotation) %>%
  mutate(nmax = max(nrate_kgha)) %>%
  filter(nrate_kgha == nmax) %>%
  select(crop, site, year, rotation, yield_kgha) %>%
  pivot_wider(names_from = rotation, values_from = yield_kgha) %>%
  mutate(cgap_max = sc-cc) %>%
  filter(!is.na(cgap_max)) %>%
  select(-cc, -sc)

use_data(sad_cgap, overwrite = TRUE)
