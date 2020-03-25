#' saf_date_to_doy
#'
#' @param mydate A date in yyyy-mm-dd format
#'
#' @return A numeric value representing the day of year, doy
#' @export
#'
saf_date_to_doy <- function(mydate = "2001-01-01"){

  year <- year(mydate)

  date1 <- paste(year, "01", "01", sep = "-")
  date2 <- paste(year, "12", "31", sep = "-")

  doy_tib <- tibble(
    date = seq(ymd(date1), ymd(date2), by = "1 day")) %>%
    mutate(doy = yday(date))

  res <- doy_tib %>%
    filter(date == mydate) %>%
    select(doy) %>%
    pull()

  return(res)
}

#' saf_doy_to_date
#'
#' @param mydoy A day of year
#' @param myyear A year if known, defaults to 2000
#'
#' @return A date in the format yyyy-mm-dd
#' @export
#'
saf_doy_to_date <- function(mydoy = 1,
                            myyear = 2000){


  date1 <- paste(myyear, "01", "01", sep = "-")
  date2 <- paste(myyear, "12", "31", sep = "-")

  doy_tib <- tibble(
    date = seq(ymd(date1), ymd(date2), by = "1 day")) %>%
    mutate(doy = yday(date))

  res <- doy_tib %>%
    filter(doy == mydoy) %>%
    select(date) %>%
    pull()

  return(res)
}
