#' saf_readapout
#'
#' @param fold_dir A character string of the file path to the folder with the APSIM out files in it
#'
#' @return A tibble with the data from all the out files (you will need to filter them)
#' @export
#'
#' @examples
#' No examples yet
saf_readapout <- function(fold_dir) {
  # read in out files created by apsim --------------------------------------

  assertthat::assert_that(is.character(fold_dir), msg = "fold_dir should be a character")

  # list the files w/out extensions (as a tibble that can be filtered)
  myrawouts <- fs::dir_ls(fold_dir) %>%
    tibble::as_tibble() %>%
    dplyr::filter(grepl("\\.out", value))


  myraws <-
    myrawouts %>%
    dplyr::rename(file = value)


  helper_readrawoutfile <- function(path) {
    suppressMessages({
      #path <- myraws[1,1] %>% pull()

      # Get names
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      myrawdatnames <- names(readr::read_table(path, skip = 2))

      # Read the actual data
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~
      myrawdat <-
        readr::read_table(path,
                   skip = 4,
                   na = "?",
                   col_names = myrawdatnames) %>%
        dplyr::mutate(Date = lubridate::dmy(Date),
               doy = lubridate::yday(Date))
    })
  }

  dat <- myraws %>%
    dplyr::mutate(path = file) %>%
    dplyr::mutate(res = path %>% purrr::map(helper_readrawoutfile)) %>%
    tidyr::unnest(cols = c(res)) %>%
    janitor::clean_names() %>%
    dplyr::mutate(file = stringr::str_extract_all(file, "\\w+(?=.out)")) %>%
    tidyr::unnest(cols = c(file))


  return(dat)
}


#mydir <- "../../../Box Sync/Gina_APSIM_modeling/sims_prelim-testing-gina/"
#library(fs)
#dir_ls(mydir)

#saf_readapout(mydir) %>%
#  select(file)


