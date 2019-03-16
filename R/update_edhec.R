#' Update the EDHEC
#'
#' Download an updated version of the Edhec Headge Fund index.
#'
#' @param class One of 'tbl', 'xts', 'zoo', 'zooreg' or 'ts'.
#'
#' @return An object of the same class required in the 'class' argument.
#'
#' @export
#'
#' @examples
#' # xts object
#' head(update_edhec('xts'))
#'
#' # tibble object
#' update_edhec('tbl')
update_edhec <- function(class = NULL) {


  # Error Handling
  if (is.null(class)) {
    .class <- "xts"
  } else {
    .class <- dplyr::ensyms(class) %>%
      as.character()
  }

  if (!(.class %in% c('tbl', 'xts', 'zoo', 'zooreg', 'ts'))) {
    stop('edhec_update does not support the ', class, ' class.')
  }

  # set up for downlaod
  safely_insistently_read <- purrr::safely(purrr::insistently(readr::read_delim))

  edhec_tbl <- safely_insistently_read(
    file          = "https://risk.edhec.edu/sites/risk/files/indices/Indices/Edhec%20Alternative%20Indices/Web/table/history.csv",
    delim         = ";",
    escape_double = FALSE,
    col_types     = readr::cols(date = readr::col_date(format = "%d/%m/%Y")),
    trim_ws       = TRUE
  )


  #
  if (is.null(edhec_tbl[["result"]])) {

    stop(
      "Download failed after 3 attempts. Please, check your internet conection and try again.",
      call. = FALSE
      )

  } else {

    # clean the tibble
    edhec_tbl <- edhec_tbl[['result']] %>%
      #dplyr::rename(index = "date") %>%
      dplyr::mutate_if(
        .predicate  = purrr::is_character,
        .funs       = stringr::str_replace,
        pattern     = '%',
        replacement = ''
      ) %>%
      dplyr::mutate_if(
        .predicate = purrr::is_character,
        .funs      = as.double
        ) %>%
      dplyr::mutate_if(
        .predicate = purrr::negate(lubridate::is.Date),
        .funs = ~ . / 100
        )

  }


  # should the tibble be coerced to something else?
  if (.class == 'tbl') {

    edhec_tbl

  } else if (.class == 'xts') {

    timetk::tk_xts(edhec_tbl, select = 2:14)

  } else if (.class == 'zoo') {

    timetk::tk_zoo(edhec_tbl, select = 2:14)

  } else if (.class == 'zooreg') {

    timetk::tk_zooreg(edhec_tbl, select = 2:14, frequency = 12, start = 1997)

  } else if (.class == 'ts') {

    edhec_tbl %>%
      timetk::tk_ts(., select = 2:14, start = 1997, frequency = 12)

  }

}
