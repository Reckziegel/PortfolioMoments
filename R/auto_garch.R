#' Fit the Best GARCH Model to an Univariate Time Series
#'
#' This function searches over different model specifications to find the best according to one of the selection criterias: Akaike, Bayes, shibata, Hannan-Quinn and likelihood.
#'
#' This function searchs thought the best GARCH models by "brute force". Since fitting a Garch's can be demanding, it is advisable to not explore all the options for portfolio optimization unless you have time and a enough memory into your computer.
#'
#' @param R A vector, matrix, data.frame, xts, timeSeries, zoo or a tibble object.
#' @param variance A vector or a list of character strings with the variance models to be computed. It can be any combination of: "sGARCH", "eGARCH", "gjrGARCH", "apARCH", "csGARCH".
#' @param distributions A vector or a list of character strings with the variance models to be computed. It can be any combination of: "norm", "std", "ged", "snorm", "sstd", "sged", "jsu", "ghyp".
#' @param garch_p,garch_q A vector or list with the number of mimimum and maximum number of lags to be included in the garch process.
#' @param arma_p,arma_q A vector or list with the mimimum and maximum number of lags to be included in the arma process.
#' @param criteria The criteria in which the models will be evaluated. One of: "Akaike", "Bayes", "Shibata", "Hannan-Quinn" and "likelihood".
#' @param n.ahead The number of periods ahead from which the sigmas should be forecasted.
#' @param conditional TRUE or FALSE. If TRUE, the the conditional sigmas covariances is returned. If FALSE, the unconditional covariance is printed.
#' @param ... Any other parameters to pass thought ugarchspec.
#'
#' @importFrom rlang .data
#'
#' @return A variance-covariance matrix.
#' @export
#'
#' @examples
auto_garch <- function(R,
                       variance = c("sGARCH", "eGARCH", "gjrGARCH", "apARCH", "csGARCH"),
                       distributions = c("norm", "std", "ged", "snorm", "sstd", "sged", "jsu", "ghyp"),
                       garch_p = c(0, 1),
                       garch_q = c(0, 1),
                       arma_p = c(0, 1),
                       arma_q = c(0, 1),
                       criteria = c("Akaike", "Bayes", "Shibata", "Hannan-Quinn", "likelihood"),
                       n.ahead = 1,
                       conditional = TRUE,
                       ...) {

  # minimal checks to avoid errors in a latter stage
  R <- PerformanceAnalytics::checkData(R)
  names <- colnames(R)
  criteria_expr <- criteria


  # start the modeling process
  tbl <- R %>%
    as.list() %>%
    tibble::enframe(.) %>%
    tidyr::crossing(variance, garch_p, garch_q, arma_p, arma_q, distributions) %>%
    dplyr::mutate(

      # corss all the specifications
      spec = purrr::pmap(
        .l  = .,
        .f  = ~ rugarch::ugarchspec(
          variance.model     = list(model = ..3, garchOrder = c(..4, ..5)),
          mean.model         = list(armaOrder = c(..6, ..7)),
          distribution.model = ..8,
          ...
        )
      ),

      # do the modeling
      model = purrr::map2(
        .x   = .data$value,
        .y   = .data$spec,
        .f   = purrr::possibly(~ rugarch::ugarchfit(spec = .y, data = .x), otherwise = NULL)
      ),

      # and try to extract the forecasts
      forecast = purrr::map(
        .x = .data$model,
        .f = purrr::possibly(~ rugarch::ugarchforecast(fitORspec = .x, n.ahead = n.ahead), otherwise = NULL)
      )
    )


  # filter the tibble by the user choosen forecast metric
  if (criteria_expr %in% c("Akaike", "Bayes", "Shibata", "Hannan-Quinn")) {

    tbl <- tbl %>%
      dplyr::mutate(infocriteria = purrr::map(
        .x = .data$model,
        .f = purrr::possibly(~ rugarch::infocriteria(.x), otherwise = NULL))
        ) %>%
      dplyr::filter(.data$infocriteria != "NULL") %>%
      dplyr::mutate(infocriteria = purrr::map(.x = .data$infocriteria, .f = as.data.frame) %>%
               purrr::map(~ tibble::rownames_to_column(., var = "criteria"))
      ) %>%
      tidyr::unnest(.data$infocriteria, .preserve = c(.data$model, .data$forecast)) %>%
      dplyr::filter(criteria == criteria_expr) %>%
      dplyr::group_by(.data$name) %>%
      dplyr::arrange(.data$V1)

  } else if (criteria == "likelihood") {

    tbl <- tbl %>%
      dplyr::mutate(
        likelihood = purrr::map(
          .x = .data$model,
          .f = purrr::possibly(~ rugarch::likelihood(.x), otherwise = NULL)
          )
        ) %>%
      dplyr::filter(.data$likelihood != "NULL") %>%
      tidyr::unnest(.data$likelihood) %>%
      dplyr::group_by(.data$name) %>%
      dplyr::arrange(.data$likelihood)

  } else {

    stop('error', call. = FALSE)

  }



  # extract and bind the sigmas
  sigma_fitted <- tbl %>%
    dplyr::slice(1) %>%
    dplyr::transmute(purrr::map(.x = .data$model, .f = rugarch::sigma)) %>%
    tibble::deframe(.) %>%
    dplyr::bind_cols()



  # sould the conditional covariance matrix be returned?
  if (conditional) {

    # extract and bind the forecasts
    sigma_forecast <- tbl %>%
      dplyr::slice(1) %>%
      dplyr::transmute(fcast = purrr::map_dbl(
        .x = .data$forecast,
        .f = ~ purrr::chuck(.x) %>%
          .@forecast %>%
          .$sigmaFor
        )
      ) %>%
      tidyr::spread(., .data$name, .data$fcast)

    out <- diag(sigma_forecast) %*% stats::cov(sigma_fitted) %*% diag(sigma_forecast)
    colnames(out) <- names
    rownames(out) <- names

  # if not, print the unconditional
  } else {

    out <- stats::cov(sigma_fitted)

  }


  return(out)

}




#   -----------------------------------------------------------------------


#
#     # extract the desired metrics (tidy the models)
#     #coefficients = purrr::map(.x = model, .f = rugarch::coef),
#     infocriteria = purrr::map(.x = model, .f = possibly(~ rugarch::infocriteria(.x), otherwise = NULL)),
#     likelihood   = purrr::map(.x = model, .f = possibly(~ rugarch::likelihood(.x), otherwise = NULL)),
#     sigma        = purrr::map(.x = model, .f = possibly( ~ rugarch::sigma(.x), otherwise = NULL))
#     #persistence  = purrr::map(.x = model, .f = rugarch::persistence),
#     #halflife     = purrr::map(.x = model, .f = rugarch::halflife),
#     #fitted       = purrr::map(.x = model, .f = rugarch::fitted),
#     #residuals    = purrr::map(.x = model, .f = rugarch::residuals)
#
