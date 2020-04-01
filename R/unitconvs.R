
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


#' saf_lbac_to_kgha
#'
#' @param dat_buac A vector of corn yields in bushels per acre
#'
#' @return A vector of corn yields in dry kilograms per hectare; adjusted for the 15% moisture the bu/ac are reported at
#' @export
#'
saf_buac_to_kgha_corn <- function(dat_buac) {
  dat_kgha <- dat_buac * 62.77 * (1-0.15)
  return(dat_kgha)

}
