#' Exponentially Weighted Moving Average (EWMA) for Returns
#'
#' This function implements EWMA for a vector of assets returns.
#'
#' @param R A vector, matrix, data.frame, xts, timeSeries or zoo object to be checked and coerced.
#' @param lambda A number between 0 and 1.
#'
#' @return A vector of assets expected returns.
#'
#' @author Bernardo Reckziegel
#'
#' @references Longerstaey, Jacques, and Martin Spencer. \emph{Riskmetrics technical document.} Morgan Guaranty Trust Company of New York: New York 51 (1996): 54.
#'
#' @export
#'
#' @examples
#' mu_ewma(EuStockMarkets, lambda = 0.96)
mu_ewma <- function(R, lambda = 0.96) {

  w  <- lambda ^ (nrow(R):1)
  w  <- w / sum(w)
  w  <- matrix(data = w, nrow = nrow(R), ncol = ncol(R), byrow = FALSE)
  mu <- colSums(w * R)

  return(mu)

}




