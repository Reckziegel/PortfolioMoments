#' Optimize Portfolios Using Custom Functions
#'
#' A wrapper around \code{optimize.portfolio} that allows the user to quickly add
#'     forecasts for mean and variance.
#'
#' @param R An xts, matrix, data frame, timeSeries, zoo or tibble object of asset returns.
#'     The data cannot be "tidy".
#' @param ... Any other paramenters to be passed into [PortfolioAnalytics::optimize.portfolio()].
#'
#' @details As explained in the vignette "Custom Moment and Objective Functions", Portfolio Analytics
#'     works more efficiently when the mean is specified as _mu_ and the variance as _sigma_.
#'
#' @return Same output of \code{optimize.portfolio}.
#'
#' @seealso [PortfolioAnalytics::optimize.portfolio()] and [purrr::as_mapper()]
#'
#' @export
#'
#' @examples
#' # load data
#' library(dplyr)
#' library(PortfolioAnalytics)
#' data(edhec)
#'
#' # create a mean-variance portfolio (long-only)
#' # that uses the \code{auto.arima} function from the \code{forecast} package as one step
#' # ahead forecasts for the mean and the famous "Bayes-Stein" estimator the covariance
#' mean_var_spec <- portfolio.spec(assets = colnames(edhec)) %>%
#'  add.constraint(portfolio = ., type = "box", min = 0.00, max = 1.00) %>%
#'  add.objective(portfolio = ., type = "risk", name = "var") %>%
#'  add.objective(portfolio = ., type = "return", name = "mean")
#'
#' optimize_portfolio(
#'     R               = edhec,
#'     portfolio       = mean_var_spec,
#'     optimize_method = 'random',
#'     search_size     = 500,
#'     mu              = forecast::auto.arima,
#'     sigma           = cov_shrink_to_bayes_stein,
#'     message         = FALSE,
#'     trace           = FALSE
#' )
#'
#'
#' # \code{PortfolioMoments} aloows you to use the same formulas syntax as \code{purrr}.
#' # Suppose you want to add restrictions to the \code{auto.arima} model above.
#'
#' \dontrun{
#' optimize_portfolio(
#'     R               = edhec,
#'     portfolio       = mean_var_spec,
#'     optimize_method = 'random',
#'     search_size     = 500,
#'     mu              = ~ forecast::auto.arima(., max.p = 2, max.q = 2, seasonal = FALSE),
#'     sigma           = cov_shrink_to_bayes_stein,
#'     message         = FALSE,
#'     trace           = FALSE
#' )
#' }
#'
optimize_portfolio <- function(R, ...) {

  UseMethod('optimize_portfolio')

}


# Default -----------------------------------------------------------------

#' @rdname optimize_portfolio
#' @export
optimize_portfolio.default <- function(R, ...) {

  stop('optimize_portfolio does not know how to deal with class ', class(R))

}


# Matrix ------------------------------------------------------------------

#' @rdname optimize_portfolio
#' @export
optimize_portfolio.matrix <- function(R, ...) {

  R <- PerformanceAnalytics::checkData(R)

  PortfolioAnalytics::optimize.portfolio(R, momentFUN = portfolio_moments, ...)

}


# xts ---------------------------------------------------------------------

#' @rdname optimize_portfolio
#' @export
optimize_portfolio.xts <- function(R, ...) {

  R <- PerformanceAnalytics::checkData(R)

  PortfolioAnalytics::optimize.portfolio(R, momentFUN = portfolio_moments, ...)

}


# zoo ---------------------------------------------------------------------

#' @rdname optimize_portfolio
#' @export
optimize_portfolio.zoo <- function(R, ...) {

  R <- PerformanceAnalytics::checkData(R)

  PortfolioAnalytics::optimize.portfolio(R, momentFUN = portfolio_moments, ...)

}


# data.frame --------------------------------------------------------------

#' @rdname optimize_portfolio
#' @export
optimize_portfolio.data.frame <- function(R, ...) {

  R <- PerformanceAnalytics::checkData(R)

  PortfolioAnalytics::optimize.portfolio(R, momentFUN = portfolio_moments, ...)

}


# tibble ------------------------------------------------------------------

#' #' @rdname optimize_portfolio
#' #' @export
#' optimize_portfolio.tbl_df <- function(R, select, date_var, silent = TRUE, ...) {
#'
#'   R <- timetk::tk_xts(data = R, select = select, date_var = date_var, silent = silent)
#'
#'   PortfolioAnalytics::optimize.portfolio(R, momentFUN = portfolio_moments, ...)
#'
#' }




