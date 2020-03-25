library(tidyverse)
library(janitor)
#library(APSIM) --let's you read in met files! but puts them in a weird s4 format. I just stole their code.

myd <- ("data-raw/sad_wea/")
fl <- list.files(myd)
fla <- fl[grepl(".met", fl)]



for (i in 1:length(fla)) {

  t.myp <- paste0(myd, fla[i])

  t.site <- (fla[i] %>% str_split(., "_") %>% unlist())[1]

  t.names <-
    read_tsv(t.myp, skip = 2) %>%
    slice(1) %>%
    unlist(., use.names = FALSE) %>%
    str_squish(.) %>%
    str_split(., " ") %>%
    unlist()

  t.data <- read.table(
    t.myp,
    skip = 5,
    header = FALSE,
    col.names = t.names,
    na.strings = c("?", "*"),
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    mutate(site = t.site)


  if (i == 1) {
    data <- t.data
  } else {
    data <- data %>% bind_rows(t.data)
  }

  i <- i + 1

}



#--make all sites only 4 characters
dclean <-
  data %>%
  mutate(site = str_sub(site, start = 1, end = 4))

sad_wea <- dclean

sad_wea %>% write_csv("data-raw/sad_wea/sad_wea.csv")
use_data(sad_wea, overwrite = T)
