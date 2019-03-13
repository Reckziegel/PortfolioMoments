#' Title
#'
#' Download an updated version of the Edhec Headge Fund index.
#'
#' @param class One of 'tbl', 'xts', 'zoo', 'zooreg' or 'ts'.
#'
#' @return An object of the same class required in the 'class' argument.
#' @export
#'
#' @examples
#' # xts object
#' update_edhec('xts') %>%
#'   head()
#'
#' # tibble object
#' update_edhec('tbl')
update_edhec <- function(class) {


  # Error Handling
  .class <- dplyr::ensyms(class) %>% as.character()

  if (!(.class %in% c('tbl', 'xts', 'zoo', 'zooreg', 'ts'))) {
    stop('edhec_update does not support the ', class, ' class.')
  }

  # set up for downlaod
  safe_read <- purrr::safely(readr::read_delim)

  edhec_tbl <- safe_read(
    file          = "https://risk.edhec.edu/sites/risk/files/indices/Indices/Edhec%20Alternative%20Indices/Web/table/history.csv",
    delim         = ";",
    escape_double = FALSE,
    col_types     = readr::cols(date = readr::col_date(format = "%d/%m/%Y")),
    trim_ws       = TRUE
  )



  # clean the tibble
  edhec_tbl <- edhec_tbl[['result']] %>%
    dplyr::rename(index = 'date') %>%
    dplyr::mutate_if(
      .predicate  = purrr::is_character,
      .funs       = stringr::str_replace,
      pattern     = '%',
      replacement = ''
    ) %>%
    dplyr::mutate_if(
      .predicate = purrr::is_character,
      .funs      = as.double
    )


  # should the tibble be coerced to something else?
  if (.class == 'tbl') {

    edhec_tbl

  } else if (.class == 'xts') {

    edhec_tbl %>%
      timetk::tk_xts(select = 2:13, date_var = index)

  } else if (.class == 'zoo') {

    edhec_tbl %>%
      timetk::tk_zoo(select = 2:13, date_var = index)

  } else if (.class == 'zooreg') {

    edhec_tbl %>%
      timetk::tk_zooreg(., select = 2:13, date_var = index, start = 1997, frequency = 12)

  } else if (.class == 'ts') {

    edhec_tbl %>%
      timetk::tk_ts(., select = 2:13, start = 1997, frequency = 12)

  }

}
