#' Exponentially Weighted Moving Average (EWMA) Covariance Matrix
#'
#' This functions implements the covariance matrix as described in Longerstaey & Spencer (1996).
#'
#' @param R A vector, matrix, data.frame, xts, timeSeries or zoo object to be checked and coerced.
#' @param lambda A number between 0 and 1.
#'
#' @return A variance-covariance matrix.
#'
#' @references Longerstaey, Jacques, and Martin Spencer. \emph{Riskmetrics technical document.} Morgan Guaranty Trust Company of New York: New York 51 (1996): 54.
#'
#' @author Bernardo Reckziegel
#'
#' @export
#'
#' @examples
#' cov_ewma(EuStockMarkets)
cov_ewma <- function(R, lambda = 0.96) {

  rows <- nrow(R)
  sample_sigma <- stats::cov(R)

  mu <- apply(R, 2, mean)
  discount_mu <- sweep(R, 2, mu, "-")

  for (i in 1:rows) {

    r <- as.double(discount_mu[i, ])
    rr <- outer(r, r)
    out <- (1 - lambda) / (1 - lambda ^ rows) * rr + (lambda * sample_sigma)

  }

  return(out)

}
