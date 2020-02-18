library(tidyverse)

sad_tidysawyer

sad_cpenH <-
  sad_tidysawyer %>%
  group_by(crop, site, rotation) %>%
  mutate(nmax = max(nrate_kgha)) %>%
  filter(nrate_kgha == nmax) %>%
  select(crop, site, year, rotation, nrate_kgha, yield_kgha) %>%
  pivot_wider(names_from = rotation, values_from = yield_kgha) %>%
  mutate(cpenH = sc-cc) %>%
  filter(!is.na(cpenH))

sad_cpen0 <-
  sad_tidysawyer %>%
  group_by(crop, site, rotation) %>%
  mutate(nmin = min(nrate_kgha)) %>%
  filter(nrate_kgha == nmin) %>%
  select(crop, site, year, rotation, yield_kgha) %>%
  pivot_wider(names_from = rotation, values_from = yield_kgha) %>%
  mutate(cpen0 = sc-cc) %>%
  filter(!is.na(cpen0))

use_data(sad_cpenH)
use_data(sad_cpen0)
