#' Shrink the Variance-Covariance Matrix towards the Bayes-Stein Estimator
#'
#' Implements the variance-covariance matrix as described in Jorrion (1986),
#'
#' @param R A vector, matrix, data.frame, xts, timeSeries, zoo or a tibble object.
#'
#' @return A variance-covariance matrix.
#'
#' @references Jorion, Philippe. \emph{Bayes-Stein Estimation For Portfolio Analysis}. Journal of Financial and Quantitative Analysis 21.3 (1986): 279-292.
#'
#' @author Bernardo Reckziegel
#'
#' @export
#'
#' @examples
#' library(PortfolioAnalytics)
#' data(edhec)
#' cov_shrink_to_bayes_stein(edhec)
cov_shrink_to_bayes_stein <- function(R) {

  R <- PerformanceAnalytics::checkData(R)

  t <- nrow(R)
  n <- ncol(R)

  mu       <- colMeans(R)
  Sigma    <- stats::cov(R)
  invSigma <- solve(Sigma)

  i <- rep(1, n)
  invSigmai   <- t(invSigma) %*% i
  w_min       <- invSigmai / as.double((t(i) %*% invSigmai))
  mu_min      <- t(mu) %*% w_min
  mu_discount <- mu - as.double(mu_min)
  invSigmaMu  <- t(invSigma) %*% mu_discount
  phi         <- (n + 2)/((n + 2) + t * (t(mu_discount) %*% invSigmaMu))
  phi         <- max(min(phi, 1), 0)

  tau   <- t * phi / (1 - phi)
  sigma <- Sigma * (1 + 1 / (t + tau)) + tau / (t * (t + 1 + tau)) * outer(i, i) / as.double((t(i) %*% invSigmai))

  return(sigma)

}

