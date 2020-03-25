library(tidyverse)
library(readxl)

sad_siteinfo <- read_excel("data-raw/sad_siteinfo/rd_siteinfo-facts.xlsx")

sad_siteinfo %>% write_csv("data-raw/sad_siteinfo/sad_siteinfo.csv")
use_data(sad_siteinfo, overwrite = TRUE)
