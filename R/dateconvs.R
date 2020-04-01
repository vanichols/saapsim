#' saf_date_to_doy
#'
#' @param mydate A date in yyyy-mm-dd format
#'
#' @return A numeric value representing the day of year, doy
#' @export
#'
saf_date_to_doy <- function(mydate = "2001-01-01"){

  year <- lubridate::year(mydate)

  date1 <- paste(year, "01", "01", sep = "-")
  date2 <- paste(year, "12", "31", sep = "-")

  doy_tib <- tibble(
    date = seq(lubridate::ymd(date1), lubridate::ymd(date2), by = "1 day")) %>%
    dplyr::mutate(doy = lubridate::yday(date))

  res <- doy_tib %>%
    dplyr::filter(date == mydate) %>%
    dplyr::select(doy) %>%
    dplyr::pull()

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
    date = seq(lubridate::ymd(date1), lubridate::ymd(date2), by = "1 day")) %>%
    dplyr::mutate(doy = lubridate::yday(date))

  res <- tibble::doy_tib %>%
    dplyr::filter(doy == mydoy) %>%
    dplyr::select(date) %>%
    dplyr::pull()

  return(res)
}
