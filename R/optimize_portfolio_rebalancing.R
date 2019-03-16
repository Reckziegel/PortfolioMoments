#' Optimize and Rebalance Portfolios Using Custom Functions
#'
#' A wrapper around \code{optimize.portfolio.rebalancing()} that allows the user to quickly add forecasts for the mean and the variance.
#'
#' @param R An xts, matrix, data frame, timeSeries, zoo or tibble object of asset returns. The data cannot be "tidy".
#' @param ... Any other paramenters to be passed into [PortfolioAnalytics::optimize.portfolio.rebalancing()].
#'
#' @details As explained in the vignette "Custom Moment and Objective Functions", Portfolio Analytics works more efficiently when the mean is specified as _mu_ and the variance as _sigma_.
#'
#' @return Same output of \code{optimize.portfolio.rebalancing}.
#'
#' @seealso [PortfolioAnalytics::optimize.portfolio.rebalancing()] and [purrr::as_mapper()]
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
#' # that uses the \code{auto.arima} function from the \code{forecast} package
#' # as one step ahead forecasts for the mean
#' # and the famous "Bayes-Stein" estimator the covariance
#' mean_var_spec <- portfolio.spec(assets = colnames(edhec)) %>%
#'  add.constraint(type = "box", min = 0.00, max = 1.00) %>%
#'  add.objective(type = "risk", name = "var") %>%
#'  add.objective(type = "return", name = "mean")
#'
#' optimize_portfolio_rebalancing(
#'     R               = edhec,
#'     portfolio       = mean_var_spec,
#'     optimize_method = 'random',
#'     rebalance_on    = "months",
#'     training_period = 120,
#'     rolling_window  = 120,
#'     search_size     = 100,
#'     mu              = forecast::ets,
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
#'     rebalance_on    = "months",
#'     training_period = 120,
#'     rolling_window  = 120,
#'     search_size     = 100,
#'     mu              = ~ forecast::auto.arima(., max.p = 2, max.q = 2, seasonal = FALSE),
#'     sigma           = cov_shrink_to_bayes_stein,
#'     message         = FALSE,
#'     trace           = FALSE
#' )
#' }
optimize_portfolio_rebalancing <- function(R, ...) {

  UseMethod('optimize_portfolio_rebalancing')

}


# Default -----------------------------------------------------------------

#' @rdname optimize_portfolio_rebalancing
#' @export
optimize_portfolio_rebalancing.default <- function(R, ...) {

  stop('optimize_portfolio_rebalancing does not know how to deal with class ', class(R), call. = FALSE)

}


# Matrix ------------------------------------------------------------------

#' @rdname optimize_portfolio_rebalancing
#' @export
optimize_portfolio_rebalancing.matrix <- function(R, ...) {

  R <- PerformanceAnalytics::checkData(R)

  PortfolioAnalytics::optimize.portfolio.rebalancing(R, momentFUN = portfolio_moments, ...)

}


# xts ---------------------------------------------------------------------

#' @rdname optimize_portfolio_rebalancing
#' @export
optimize_portfolio_rebalancing.xts <- function(R, ...) {

  R <- PerformanceAnalytics::checkData(R)

  PortfolioAnalytics::optimize.portfolio.rebalancing(R, momentFUN = portfolio_moments, ...)

}


# zoo ---------------------------------------------------------------------

#' @rdname optimize_portfolio_rebalancing
#' @export
optimize_portfolio_rebalancing.zoo <- function(R, ...) {

  R <- PerformanceAnalytics::checkData(R)

  PortfolioAnalytics::optimize.portfolio.rebalancing(R, momentFUN = portfolio_moments, ...)

}


# data.frame --------------------------------------------------------------

#' @rdname optimize_portfolio_rebalancing
#' @export
optimize_portfolio_rebalancing.data.frame <- function(R, ...) {

  R <- PerformanceAnalytics::checkData(R)

  PortfolioAnalytics::optimize.portfolio.rebalancing(R, momentFUN = portfolio_moments, ...)

}


# tibble ------------------------------------------------------------------
##
##' # rdname optimize_portfolio_rebalancing
##' # export
##' optimize_portfolio_rebalancing.tbl_df <- function(R, select, date_var, silent = TRUE, ...) {
##'
##'   R <- timetk::tk_xts(data = R, select = select, date_var = date_var, silent = silent)
##'
##'   PortfolioAnalytics::optimize.portfolio.rebalancing(R, momentFUN = portfolio_moments, ...)
##'
##' }

