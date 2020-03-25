#########################
# purpose: get mitch's data in long form
# created: jan 16 2020
# author: gina
# last modified: march 25 2020 (moved to pkg)
#
# notes:
#########################


rm(list = ls())
library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(saapsim) #--for conversion function

# raw mitch data ----------------------------------------------------------

mit <- read_csv("data-raw/sad_tidysawyer/mitch_LTN-yields-allNrates.csv")



# tidy, make metric units -------------------------------------------------

ylds <-
  mit %>%
  clean_names() %>%   #<-- makes col names lowercase
  select(crop:x240, x60, x180) %>%    #<-- grab x60 and x180 at the end
  gather(x0:x180, key = nrate_lbsac, value = yield_buac) %>%
  mutate_if(is.character, tolower) %>%
  mutate(site = str_sub(site, 1, 4),
         nrate_lbac = as.numeric(str_sub(nrate_lbsac, 2)),
         nrate_kgha = saf_lbac_to_kgha(nrate_lbac), #<-- use saf conversions
         yield_kgha = saf_buac_to_kgha_corn(yield_buac)) %>%
  select(crop, site, year, rotation, nrate_kgha, yield_kgha) %>%
  arrange(crop, site, year, rotation, nrate_kgha) %>%
  filter(!is.na(yield_kgha)) %>%
  filter(crop == "corn")


# read in standard devs ---------------------------------------------------

ysd <- read_csv("data-raw/sad_tidysawyer/mitch_standard-devs-yields.csv") %>%
  filter(crop != "soybean") %>%
  mutate(site = str_to_lower(site),
         site = str_sub(site, 1, 4),
         rotation = str_to_lower(rotation),
         sd_kgha = saf_lbac_to_kgha(sd_buac),
         nrate_kgha = saf_lbac_to_kgha(NR)) %>%
  select(-NR, -sd_buac)



# merge em ----------------------------------------------------------------

sad_tidysawyer <-
  ylds %>%
  left_join(ysd)

sad_tidysawyer %>% write_csv("data-raw/sad_tidysawyer/sad_tidysawyer.csv")
use_data(sad_tidysawyer, overwrite = TRUE)
