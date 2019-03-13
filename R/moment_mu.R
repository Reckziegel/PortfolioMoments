
# Use Method --------------------------------------------------------------

moment_mu <- function(R, fun, ...) {

  UseMethod('moment_mu')

}



# Defaul ------------------------------------------------------------------

moment_mu.default <- function(R, fun, ...) {

  stop('moment_mu does not know how to deal with class ', class(R))

}



# Matrix ------------------------------------------------------------------

moment_mu.matrix <- function(R, fun, h = 1, ...) {

  # tidy eval
  fun_mapper <- purrr::as_mapper(fun)
  dots_expr  <- dplyr::enquos(...)

  mu <- R %>%

    # adjust for the tidyverse
    dplyr::as_tibble() %>%
    tidyr::gather(., key = 'key', value = 'value') %>%
    tidyr::nest(value) %>%

    # model
    dplyr::mutate(
      model = purrr::map(.x = data, .f = fun_mapper, !!! dots_expr) %>%
        purrr::map(.f = forecast::forecast, h = h) %>%
        purrr::map(.f = sweep::sw_sweep)
    ) %>%

    # adjust for portfolio analytics
    tidyr::unnest(model) %>%
    dplyr::filter(complete.cases(.)) %>%
    dplyr::filter(index == max(index)) %>%
    dplyr::select(value) %>%
    dplyr::rename(mu = 'value') %>%
    as.matrix()


  if (!purrr::is_null(colnames(R))) {
    colnames(mu) <- names(R)
  }

  out <- list(mu = mu)

  return(out)

}



# xts ---------------------------------------------------------------------

moment_mu.xts <- function(R, fun, h = 1, ...) {

  # tidy eval
  fun_mapper <- purrr::as_mapper(fun)
  dots_expr  <- dplyr::quos(...)

  mu <- R %>%

    # adjust for the tidyverse
    timetk::tk_tbl(.) %>%
    tidyr::gather(., key = 'key', value = 'value', -index) %>%
    tidyr::nest(index, value) %>%

    # model
    dplyr::mutate(
      model = purrr::map(.x = data, .f = timetk::tk_xts, select = value, date_var = index) %>%
        purrr::map(.f = fun_mapper, !!! dots_expr) %>%
        purrr::map(.f = forecast::forecast, h = h) %>%
        purrr::map(.f = sweep::sw_sweep)
      ) %>%

    # adjust for portfolio analytics
    tidyr::unnest(model) %>%
    dplyr::filter(complete.cases(.)) %>%
    dplyr::filter(index == max(index)) %>%
    dplyr::select(value) %>%
    dplyr::rename(mu = 'value') %>%
    as.matrix()


  if (!purrr::is_null(colnames(R))) {
    rownames(mu) <- names(R)
  }

  out <- list(mu = mu)
  return(out)

}



# zoo ---------------------------------------------------------------------

moment_mu.zoo <- function(R, fun, h = 1, ...) {

  # tidy eval
  fun_mapper <- purrr::as_mapper(fun)
  dots_expr  <- dplyr::quos(...)

  mu <- R %>%

    # adjust for the tidyverse
    timetk::tk_tbl(.) %>%
    tidyr::gather(., key = 'key', value = 'value', -index) %>%
    tidyr::nest(index, value) %>%

    # model
    dplyr::mutate(
      model = purrr::map(.x = data, .f = timetk::tk_xts, select = value, date_var = index) %>%
        purrr::map(.f = fun_mapper, !!! dots_expr) %>%
        purrr::map(.f = forecast::forecast, h = h) %>%
        purrr::map(.f = sweep::sw_sweep)
    ) %>%

    # adjust for portfolio analytics
    tidyr::unnest(sweep) %>%
    dplyr::filter(complete.cases(.)) %>%
    dplyr::filter(index == max(index)) %>%
    dplyr::select(value) %>%
    dplyr::rename(mu = 'value') %>%
    as.matrix()


  if (!purrr::is_null(colnames(R))) {
    rownames(mu) <- names(R)
  }

  out <- list(mu = mu)
  return(out)

}



# ts ----------------------------------------------------------------------

moment_mu.ts <- function(R, fun, h = 1, ...) {

  # tidy eval
  fun_mapper <- purrr::as_mapper(fun)
  dots_expr  <- dplyr::quos(...)

  mu <- R %>%

    # adjust for the tidyverse
    dplyr::as_tibble() %>%
    tibble::rownames_to_column(., var = 'index') %>%
    dplyr::mutate_if(purrr::is_character, as.numeric) %>%
    dplyr::mutate(index = lubridate::as_date(index)) %>%
    tidyr::gather(., key = 'key', value = 'value', -index) %>%
    tidyr::nest(index, value) %>%

    # model
    dplyr::mutate(
      model = purrr::map(.x = data, .f = timetk::tk_xts, select = value, date_var = index) %>%
        purrr::map(.f = fun_mapper, !!! dots_expr) %>%
        purrr::map(.f = forecast::forecast, h = h) %>%
        purrr::map(.f = sweep::sw_sweep)
      ) %>%

    # adjust for portfolio analytics
    tidyr::unnest(model) %>%
    dplyr::filter(complete.cases(.)) %>%
    dplyr::filter(index == max(index)) %>%
    dplyr::select(value) %>%
    dplyr::rename(mu = 'value') %>%
    as.matrix()


  if (!purrr::is_null(colnames(R))) {
    rownames(mu) <- names(R)
  }

  out <- list(mu = mu)
  return(out)

}



# tibble ------------------------------------------------------------------

moment_mu.tbl <- function(R, fun, h = 1, ...) {

  # tidy eval
  fun_mapper <- purrr::as_mapper(fun)
  dots_expr  <- dplyr::enquos(...)

  mu <- R %>%

    # adjust for the tidyverse
    timetk::tk_tbl() %>%
    tidyr::gather(., key = 'key', value = 'value', -index) %>%
    tidyr::nest(index, value) %>%

    # model
    dplyr::mutate(
      model = purrr::map(.x = data, .f = timetk::tk_xts, select = value, date_var = index) %>%
        purrr::map(.f = fun_mapper, !!! dots_expr) %>%
        purrr::map(.f = forecast::forecast, h = h) %>%
        purrr::map(.f = sweep::sw_sweep)
    ) %>%

    # adjust for portfolio analytics
    tidyr::unnest(model) %>%
    dplyr::filter(complete.cases(.)) %>%
    dplyr::filter(index == max(index)) %>%
    dplyr::select(value) %>%
    dplyr::rename(mu = 'value') %>%
    as.matrix()


  if (!purrr::is_null(colnames(R))) {

    for (i in names(R[2:14])) {

      rownames(mu[[i]], do.NULL = FALSE) <- colnames(R[[i]])

    }

  }

  #out <- list(mu = mu)
  return(mu)

}


