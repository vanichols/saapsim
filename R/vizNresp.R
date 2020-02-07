#' Combine experimental and predicted data
#'
#' @param exp_data Experimental data, see sad_exdata for format example, yield must be in kg/ha
#' @param pred_data Predicted data, output from saf_predNresp, yield must be in kg/ha
#'
#' @return A tibble with columns named data_type (pred or exp) and yield_kgha
#' @export
saf_combN <- function(exp_data, pred_data) {
  assertthat::assert_that("yield_kgha" %in% colnames(exp_data) == TRUE,
                          msg = "Use the experimental data for exp_data\nI'm making you use kg/ha units for yield (col must be named yield_kgha)")

  assertthat::assert_that("pred_kgha" %in% colnames(pred_data) == TRUE,
                          msg = "Use output from the saf_predNresp function for pred_data\nI'm making you use kg/ha units for yield")

  # change col name in exp_data
  exp_data1 <- exp_data %>%
    dplyr::rename(exp_kgha = yield_kgha) %>%
    dplyr::mutate(nrate_kgha = round(nrate_kgha, 0))

  # merge the two datasets and pivot
  comb <-
    pred_data %>%
    dplyr::select(site, year, rotation, model, nrate_kgha, pred_kgha) %>%
    dplyr::left_join(exp_data1) %>%
    # tidyr::pivot_longer(cols = c("pred_kgha", "exp_kgha"),
    #              values_to = "yield_kgha",
    #              names_to = "type") %>%
    # tidyr::separate(type, into = c("data_type", "units")) %>%
    # dplyr::select(-units) %>%
    tidyr::fill(crop) #%>%
    #dplyr::filter(!is.na(yield_kgha))

  return(comb)

}

#' Quick viz of experimental data points (field or apsim) and statistical model preds
#'
#' @param comb_data Output from saf_combN
#'
#' @return A plot faceted by model type and site
#' @export
saf_vizN <- function(comb_data) {
  assertthat::assert_that("pred_kgha" %in% colnames(comb_data) == TRUE,
                          msg = "Use output from saf_combN")
  assertthat::assert_that("exp_kgha" %in% colnames(comb_data) == TRUE,
                          msg = "Use output from saf_combN")

  fig1 <-
    comb_data %>%
    ggplot() +
    geom_point(aes(nrate_kgha, exp_kgha), color = "black", size = 2) +
    geom_line(aes(nrate_kgha, pred_kgha), color = "red2") +
    facet_grid(model ~ site) +
    theme_bw()

  return(fig1)

}
