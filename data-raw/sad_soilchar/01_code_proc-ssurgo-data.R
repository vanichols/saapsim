# Created:       3/25/2020
# last edited:   3/26/2020 (upgrade from practice to actual lat/lon)
#
# purpose: use feddata package to get ssurgo data?
# notes:


rm(list = ls())
#devtools::install_github("vanichols/saapsim", force = T)
library(saapsim) #--my package, has his data in it :P
library(tidyverse)
library(janitor)
library(lubridate)
library(sf)
library(glue)





# a helper function -------------------------------------------------------


fun_summarize_ssurgo <- function(f_mychoice = "ames") {

  ########--for trouble
  #f_mychoice <- "kana"

  my_dir <- glue('data-raw/sad_soilchar/EXTRACTIONS/{f_mychoice}/SSURGO/') %>% as.character()
  my_shp <-
    glue('{my_dir}{f_mychoice}_SSURGO_Mapunits.shp') %>% as.character()
  my_mu <-
    glue('{my_dir}{f_mychoice}_SSURGO_mapunit.csv') %>% as.character()
  my_cmpnt <-
    glue('{my_dir}{f_mychoice}_SSURGO_component.csv') %>% as.character()
  my_chor <-
    glue('{my_dir}{f_mychoice}_SSURGO_chorizon.csv') %>% as.character()

  # look at what we got -----------------------------------------------------

  #srgo_shp <- st_read(my_shp)
  #ggplot() + geom_sf(data = srgo_shp, aes(fill = MUKEY))


  # read in csv files -------------------------------------------------------

  # 1. mapunit descriptions

  mu_dat <- read_csv(my_mu) %>%
    remove_empty("cols") #--removes empty columns


  mu_major <-
    mu_dat %>%
    filter(muacres == max(muacres)) %>%
    select(mukey)

  mu_wgts <-
    mu_dat %>%
    mutate(tot_ac = sum(muacres),
           mu_wgt = muacres/tot_ac) %>%
    select(mukey, mu_wgt)


  # 2. component, tells what each mukey is 'made up of', has mukey and cokey
  cmpnt_dat <- read_csv(my_cmpnt) %>%
    select(mukey, cokey, everything())

  cmpnt_major <-
    cmpnt_dat %>%
    filter(majcompflag == "Yes")

  # 3. chor, has horizon depths
  # .r means reference value, .l means low value, .h means high value

  chor_dat <- read_csv(my_chor)


  #--Drainage class
  # Use major component and major map unit

  draincl_maj <-
    mu_major %>%
    left_join(cmpnt_major) %>%
    select(drainagecl) %>%
    rename(draincl_maj = drainagecl)

  #--Crop prod index

  # Major component and major map unit
  prodindx_maj <-
    mu_major %>%
    left_join(cmpnt_major) %>%
    select(cropprodindex) %>%
    rename(cropprodindex_maj = cropprodindex)

  # Major comp, weighted map unit
  prodindx_wgt <-
      mu_wgts %>%
      left_join(cmpnt_major) %>%
      select(mukey, mu_wgt, cropprodindex) %>%
        mutate(val = mu_wgt * cropprodindex) %>%
        summarise(cropprodindex_wgt = sum(val))



  #--depth to b horizon

  ahorz_dat <-
    chor_dat %>%
    select(cokey,
           chkey,
           hzname,
           desgnmaster,
           contains(".r") & contains("hz")) %>%
    filter(desgnmaster == "A") %>%
    #--average by cokey (?)
    group_by(cokey) %>%
    summarise(depth_to_Bhz_cm = mean(hzdepb.r, na.rm = T))

  # use major cmpnt, and major mapunit
  hzb_maj <-
    mu_major %>%
    left_join(cmpnt_major) %>%
    left_join(ahorz_dat) %>%
    select(depth_to_Bhz_cm) %>%
    rename(bhz_maj = depth_to_Bhz_cm)

  # use weighted cmpnts and mapunits

  hzb_wgt <-
    cmpnt_dat %>%
    select(mukey, cokey, comppct.r)  %>%
    left_join(ahorz_dat) %>%
    group_by(mukey) %>%
    mutate(
      comp_tot = sum(comppct.r),
      wgt = comppct.r / comp_tot,
      val = depth_to_Bhz_cm * wgt
    ) %>%
    summarise(bhz_wt = sum(val)) %>%
    left_join(mu_dat) %>%
    filter(muacres == max(muacres)) %>%
    select(bhz_wt)


  #--iacsr
  # major mapunit

  iasr_maj <-
    mu_major %>%
    left_join(mu_dat) %>%
    select(iacornsr) %>%
    rename(iacsr_maj = iacornsr)


  # weighted average
  iasr_wgt <-
    mu_wgts %>%
    left_join(mu_dat) %>%
    mutate(
      val = mu_wgt * iacornsr
    ) %>%
    summarise(iacsr_wgt = sum(val))

  dat <-
    bind_cols(iasr_maj, iasr_wgt,
              hzb_maj, hzb_wgt,
              draincl_major,
              prodindx_maj, prodindx_wgt) %>%
    mutate(site = f_mychoice)

  return(dat)
}



# use function ------------------------------------------------------------


# use pre-downloaded ssurgo data ------------------------------------------

sll <- sad_siteinfo %>%
  select(site) %>%
  pull()

mychoice <- sll[1]
mychoice

dat <- fun_summarize_ssurgo(f_mychoice = mychoice)


for (i in 2:length(sll) ) {

  mychoice <- sll[i]
  f_dat <- fun_summarize_ssurgo(f_mychoice = mychoice)

  dat <- bind_rows(dat, f_dat)

   i <- i + 1
}

dat %>%
  select(site, everything()) %>%
  write_csv("data-raw/sad_soilchar/td_ssurgo-vals.csv")


