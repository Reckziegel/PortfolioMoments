#' Shrink The Covariance Matrix towards to the Market Factor
#'
#' Implements the covariance matrix as described in Ledoit & Wolf (2003).
#'
#' @param R A vector, matrix, data.frame, xts, timeSeries or zoo object to be checked and coerced.
#' @param shrink A number between 0 and 1.
#' @param portfolio If \code{TRUE} only the covariance matrix is printed. If \code{FALSE} a list is returned with the covariance-matrix and the shrinkage intensity.
#'
#' @return A variance-covariance matrix.
#'
#' @references Ledoit, Olivier, and Michael Wolf. \emph{Improved estimation of the covariance matrix of stock returns with an application to portfolio selection.} Journal of empirical finance 10.5 (2003): 603-621.
#'
#' @author Bernardo Reckziegel
#'
#' @export
#'
#' @examples
#' cov_shrink_to_market(EuStockMarkets)
cov_shrink_to_market <- function(R, shrink = TRUE, portfolio = TRUE) {

  # de-mean returns
  t     <- nrow(R)
  n     <- ncol(R)
  meanx <- colMeans(R)
  R     <- sweep(R, 2, meanx, "-")
  xmkt  <- rowMeans(R)

  sample <- stats::cov(cbind(R, xmkt)) * (t - 1) / t
  covmkt <- sample[1:n, n + 1]
  varmkt <- sample[n + 1, n + 1]
  sample <- sample[1:n, 1:n]
  prior  <- (covmkt %*% t(covmkt)) / varmkt
  diag(prior) <- diag(sample)

  if (shrink) {

    # compute shrinkage parameters
    c <- norm(sample - prior, type = 'F') ^ 2
    y <- as.matrix(R ^ 2)
    p <- (1 / t) * sum(colSums(t(y) %*% y)) - sum(colSums(sample ^ 2))

    # r is divided into diagonal and off-diagonal terms, and the off-diagonal term is itself divided into smaller terms
    rdiag <- (1 / t) * sum(colSums(y ^ 2)) - sum(diag(sample) ^ 2)
    z     <- R * matrix(rep(xmkt, n), ncol = n)
    v1    <- (1 / t) * t((t(z) %*% y)) - matrix(rep(covmkt, n), ncol = n) * sample
    roff1 <- (sum(colSums(v1 * t(matrix(rep(covmkt, n), ncol = n)))) / varmkt) - (sum(diag(v1) * covmkt) / varmkt)
    v3    <- (1 / t) * t(z) %*% z - varmkt * sample
    roff3 <- sum(colSums(v3 * (covmkt %*% t(covmkt)))) / varmkt ^ 2 - sum(diag(v3) * covmkt ^ 2) / varmkt ^ 2
    roff  <- 2 * roff1 - roff3
    r     <- rdiag + roff

    # compute shrinkage constant
    k         <- (p - r) / c
    shrinkage <- max(0, min(1, k / t))

  } else {

    # use specified number
    shrinkage <- shrink

  }

  # compute the estimator
  sigma <- shrinkage * prior + (1 - shrinkage) * sample
  colnames(sigma) <- colnames(R)
  rownames(sigma) <- colnames(R)

  # output
  if (portfolio) {
    out <- sigma
  } else {
    out <- list(sigma = sigma, shrinkage = shrinkage)
  }

  return(out)

}

