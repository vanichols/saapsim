library(tidyverse)
library(saapsim)

sad_tidysawyer

exp <-
  sad_tidysawyer %>%
  mutate(data_type = "exp_vals",
         year = as.character(year))

preds <-
  sad_tidysawyer %>%
  group_by(site, rotation) %>%
  nest() %>%
  saf_fitNresp() %>%
  filter(model == "QP") %>%
  saf_predNresp() %>%
  rename(yield_kgha = pred_kgha) %>%
  mutate(year = "1999-2016",
         data_type = "exp_pred") %>%
  select(-model, -fit)

sad_qppreds <- exp %>% bind_rows(preds)

use_data(sad_qppreds, overwrite = TRUE)

