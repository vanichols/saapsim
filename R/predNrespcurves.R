

#' saf_predNresp
#'
#' @param mydata Results from saf_fitNresp() function
#' @param highN Highest rate of nitrogen application (in kg/ha) you want to predict (default is 270)
#'
#' @return A tibble with the predictions for 0-highN by 1 kg/ha
#' @export
#'
saf_predNresp <- function(mydata, highN = 270) {
  assertthat::assert_that(is.tibble(mydata), msg = "Make your data a tibble")
  assertthat::assert_that("fit" %in% colnames(mydata) == TRUE,
                          msg = "Make sure you are using the output from the saf_fitNresp function")

  helper_LQLP <- function(LQLPdata, highN) {
    LQLPres <-
      LQLPdata %>%
      dplyr::mutate(nrate_kgha = list(0:highN)) %>%
      dplyr::mutate(pred_kgha = fit %>%
                      purrr::map(predict, newdata = data.frame(nrate_kgha = 0:highN))) %>%
      dplyr::select(-data) %>%
      tidyr::unnest(cols = c(nrate_kgha, pred_kgha))
    return(LQLPres)
  }

  helper_QP <- function(QPdata, highN) {
    QPres <- QPdata %>%
      dplyr::mutate(nrate_kgha = list(0:highN)) %>%
      dplyr::mutate(pred_kgha = fit %>%
                      purrr::map(predict, newdata = data.frame(nrate_kgha = 0:highN))) %>%
      dplyr::select(-data) %>%
      tidyr::unnest(cols = c(nrate_kgha, pred_kgha)) %>%
      dplyr::mutate(pred_kgha = as.numeric(pred_kgha))
    return(QPres)
  }

  LQLPdata1 <-
    LQLPdata %>%
    filter(model %in% c("linear", "quad", "LP")) %>%
    helper_LQLP()


  LQLPdata2 <-
    filter(model %in% c("QP")) %>%
    helper_QP()

  res <- bind_rows(LQLPdata1, LQLPdata2)

  return(res)

}
