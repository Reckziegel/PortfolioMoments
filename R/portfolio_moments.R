#' Specify custom functions to be used with Portfolio Analytics
#'
#' \code{portfolio_moments} uses \code{purrr} syntax for formulas. This enables a flexible and succinct way to define anonymous (lambda) functions.
#'
#' \code{mu}, \code{sigma}, \code{m3} and \code{m4} can be a passed as a function, a formula or a vector:
#'
#'    \itemize{
#'      \item If a __function__: use it as it is.
#'      \item If a __formula__: e.g. \code{~ . + 2} is converted to a function. If the function requires a time series to run refer to it with \code{.}.
#'      \item If a __vector__ e.g. \code{~ 0.01} is converted to a function and all forecasts get the same value, 0.01.
#'    }
#'
#' \code{R}, the first argument of \code{portfolio_moments}, does not need to be specified in \code{\link{optimize_portfolio}}. Use only \code{mu}, \code{sigma}, \code{m3} and \code{m4}.
#'
#' @param R A vector, matrix, data.frame, xts, timeSeries, zoo or a tibble object.
#' @param mu,sigma,m3,m4  A function, formula, or vector (not necessarily atomic). Follows the same syntax as the \code{\link[purrr:map]{map}} family of functions.
#'
#' @return A list with the suplied components.
#'
#' @seealso [purrr::map()], [purrr::as_mapper()], [[rlang::as_function()] and [PortfolioAnalytics::optimize.portfolio()]
#'
#' @author Bernardo Reckziegel
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(PortfolioAnalytics)
#' data(edhec)
#'
#' # function format:
#' portfolio_moments(
#'     R     = edhec,
#'     mu    = forecast::auto.arima,
#'     sigma = cov_shrink_to_market
#' )
#'
#' # formula format: use to customize your call
#' # (ps: works for "sigma" as well)
#' portfolio_moments(
#'     R     = edhec,
#'     mu    = ~ forecast::auto.arima(y = ., max.p = 2, max.q = 2, stationary = TRUE),
#'     sigma = cov_shrink_to_market
#' )
#'
#' # vector format:
#' portfolio_moments(
#'     R     = edhec,
#'     mu    = ~ 0.01, # this estimator is 100% bias and 0% variance.
#'     sigma = cov_shrink_to_market
#' )
#'
#'
#' # When optimizing the portfolio use "mu" and "sigma" only.
#' mean_var_pspec <- portfolio.spec(assets = colnames(edhec)) %>%
#'  add.constraint(portfolio = ., type = "box", min = 0.00, max = 1.00) %>%
#'  add.objective(portfolio = ., type = "risk", name = "var") %>%
#'  add.objective(portfolio = ., type = "return", name = "mean")
#'
#'  optimize_portfolio(
#'     R               = edhec,
#'     portfolio       = mean_var_pspec,
#'     optimize_method = 'random',
#'     search_size     = 500,
#'     mu              = forecast::ets, # no need to use portfolio_moments()
#'     sigma           = stats::cov,    # just "mu" and "sigma"
#'     message         = FALSE,
#'     trace           = FALSE
#' )
#' # Note that the sample-mean and the sample-covariance are also accepted.
portfolio_moments <- function(R, mu = NULL, sigma = NULL, m3 = NULL, m4 = NULL) {

  out <- list()

  if (!is.null(mu)) {
    .mu_mapper <- rlang::as_function(mu)
    out$mu <- R %>%
      as.list() %>%
      purrr::map(.mu_mapper) %>%
      purrr::map(forecast::forecast, h = 1) %>%
      purrr::map_dbl(purrr::chuck, 'mean')
  } #else {
    #out$mu <- colMeans(R)
  #}

  if (!is.null(sigma)) {
    .sigma_mapper <- rlang::as_function(sigma)
    out$sigma <- .sigma_mapper(R)
  } #else {
    #out$sigma <- cov(R)
  #}

  if (!is.null(m3)) {
    .m3_mapper <- rlang::as_function(m3)
    out$m3 <- .m3_mapper(R)
  }

  if (!is.null(m4)) {
    .m4_mapper <- rlang::as_function(m4)
    out$m4 <- .m4_mapper(R)
  }

  return(out)

}

