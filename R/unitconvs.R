
#' saf_lbac_to_kgha
#'
#' @param dat_lbac A vector of values in pounds per acre
#'
#' @return A vector of values in kilograms per hectare; multiplied by 1.123
#' @export
#'
saf_lbac_to_kgha <- function(dat_lbac) {
  dat_kgha <- dat_lbac *1.123
  return(dat_kgha)

}


#' saf_buac_to_kgha_corn
#'
#' @param dat_buac A vector of corn yields in bushels per acre
#'
#' @return A vector of corn yields in dry kilograms per hectare; adjusted for the 15% moisture the bu/ac are reported at
#' @export
#'
saf_buac_to_kgha_corn <- function(dat_buac) {
  dat_kgha <- dat_buac * (1-0.155) * 56 * (0.453592) * (2.47105)
  return(dat_kgha)

}


#' saf_kgha_to_buac_corn
#'
#' @param dat_kgha A vector of corn yields in bushels per acre
#'
#' @return A vector of corn yields in bushels per acre; adjusted for the 15% moisture the bu/ac are reported at
#' @export
#'
saf_kgha_to_buac_corn <- function(dat_kgha) {
  dat_buac <- dat_kgha * (0.404686) * (2.20462) * 1/(1-0.155) * (1/56)
  return(dat_buac)

}
