
#' saf_fitNresp
#'
#' @param mydata The N resp data that has been grouped (site, year, rotation) and nested. See the included data 'sad_exdata' for an example
#'
#' @return A tibble with the data and fits nested
#' @export
#'
saf_fitNresp <- function(mydata) {
  assertthat::assert_that(is.tibble(mydata), msg = "Make your data a tibble")
  assertthat::assert_that("data" %in% colnames(mydata) == TRUE,
                          msg = "Data should be in nested format with column named 'data'. Did you group_by?\nSuggested grouping: site, year, rotation.")

  ## fit 4 fucntions
  myfits <-
  mydata %>%
  ## linear function
  mutate(linear = purrr::map(data, ~ try(lm(yield_kgha ~  nrate_kgha,
                                            data =.))))%>%
  ## quadratic funtion
  mutate(quad = purrr::map(data, ~ try(lm(yield_kgha ~  nrate_kgha + I(nrate_kgha^2),
                                          data =.))))%>%

  ## linear plateau function
  mutate(LP = purrr::map(data, ~ try(stats::nls(yield_kgha ~ a + b * (nrate_kgha - c) * (nrate_kgha <= c),
                                         start = list(a = 7000, b = 640, c = 200),
                                         data = .))))%>%
  ## quad plateau function
  mutate(QP = purrr::map(data, ~ try(stats::nls(yield_kgha ~ (a + b * nrate_kgha + c * I(nrate_kgha^2)) *
                                           (nrate_kgha <= -0.5 * b/c) +
                                           (a + I(-b^2/(4 * c))) * (nrate_kgha > -0.5 * b/c),
                                         start = list(a =50, b = 1, c = -0.00024),
                                         data = .)))) %>%
  tidyr::gather(linear:QP, key = model, value = fit) %>%
  dplyr::mutate(newfit = as.character(fit),
         newfit = stringr::str_sub(newfit, 1, 3))%>%
  dplyr::filter(newfit != "Err")%>%
  dplyr::select(-newfit)

  return(myfits)

  }


#' saf_smyNfits
#'
#' @param myfits The resulting tibble from using the saf_fitNresp function
#'
#' @return A tibble (nothing nested) with the fitted parameters for each model, plus the R2 value
#' @export
#'
saf_smyNfits <- function(myfits) {

  assertthat::assert_that("data" %in% colnames(myfits) == TRUE,
                          msg = "Run the saf_fitNresp function first")
  assertthat::assert_that("model" %in% colnames(myfits) == TRUE,
                        msg = "Run the saf_fitNresp function first")
  assertthat::assert_that("fit" %in% colnames(myfits) == TRUE,
                          msg = "Run the saf_fitNresp function first")
  assertthat::assert_that(is_tibble(myfits) == TRUE,
                          msg = "Feed the output from the saf_fitNresp function")

  helper_find_r2_fun <- function(myfit) {
    myl <- length(fitted(myfit)) -1
    sqt <- var( fitted(myfit) + resid(myfit) ) * myl
    r1 <- (sqt - deviance(myfit)) / sqt
    r1 <- round(r1, 4)
    return(r1)
  }

    mysmy <-
      myfits %>%
      #--extract a parm (?intercept?)
      dplyr::mutate(
        a = dplyr::case_when(
          model == "linear" ~ purrr::map(fit, .f = ~ coef(.)[2]),
          model == "quad" ~ purrr::map(fit, .f = ~ coef(.)[1]),
          model == "LP" ~ purrr::map(fit, .f = ~ coef(.)[1]),
          model == "QP" ~ purrr::map(fit, .f = ~ coef(.)[1])
        )
      ) %>%
      #--extract b parm (slope, kind of?)
      dplyr::mutate(
        b = dplyr::case_when(
          model == "linear" ~ purrr::map(fit, .f = ~ coef(.)[1]),
          model == "quad" ~ purrr::map(fit, .f = ~ coef(.)[2]),
          model == "LP" ~ purrr::map(fit, .f = ~ coef(.)[2]),
          model == "QP" ~ purrr::map(fit, .f = ~ coef(.)[2])
        )
      ) %>%
      #--extract c parm (?)
      dplyr::mutate(
        c = dplyr::case_when(
          model == "linear" ~ purrr::map(fit, .f = ~ coef(.)[3]), #--this just produces an NA
          model == "quad" ~ purrr::map(fit, .f = ~ coef(.)[3]),
          model == "LP" ~ purrr::map(fit, .f = ~ coef(.)[3]),
          model == "QP" ~ purrr::map(fit, .f = ~ coef(.)[3])
          #TRUE ~ NA #--this doesn't work for some reason
        )
      ) %>%
      #--find R2
      dplyr::mutate(r2 = fit %>% purrr::map(helper_find_r2_fun)) %>%
      dplyr::select(-data,-fit) %>%
      tidyr::unnest(cols = c(a, b, c, r2))


    return(mysmy)
  }


