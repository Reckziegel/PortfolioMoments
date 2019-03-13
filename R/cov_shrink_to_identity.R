#' Shrink the Covariance Matrix towards to Identity
#'
#' Implements the covariance matrix as described in Ledoit & Wolf (2004).
#'
#' @param R A vector, matrix, data.frame, xts, timeSeries or zoo object to be checked and coerced.
#' @param shrink A number between 0 and 1.
#' @param portfolio If \code{TRUE} only the covariance matrix is printed. If \code{FALSE} a list is returned with the covariance-matrix and the shrinkage intensity.
#'
#' @return A variance-covariance matrix.
#'
#' @references Ledoit, Olivier, and Michael Wolf. \emph{A well-conditioned estimator for large-dimensional covariance matrices.} Journal of multivariate analysis 88.2 (2004): 365-411.
#'
#' @author Bernardo Reckziegel
#'
#' @export
#'
#' @examples
#' cov_shrink_to_identity(EuStockMarkets)
#' cov_shrink_to_identity(EuStockMarkets, portfolio = FALSE)
cov_shrink_to_identity <- function(R, shrink = NULL, portfolio = TRUE) {

  if (is.numeric(shrink)) stopifnot(shrink >= 0 & shrink <= 1)

  # de-mean returns
  t <- nrow(R)
  n <- ncol(R)
  meanx <- colMeans(R)
  new_R <- sweep(R, 2, meanx, "-")

  # compute the covariance matrix
  cov <- (1 / t) * (t(new_R) %*% new_R)

  # compute the prior
  meanvar <- mean(diag(cov))
  prior <- meanvar * diag(n)

  # compute the shrinkage parameters
  if (purrr::is_null(shrink)) {

    # what they call "p"
    y <- new_R ^ 2
    phiMat <- (t(y) %*% y) / t - (cov ^ 2)
    phi <- sum(colSums(phiMat))

    # ps: what they call "r" is not needed for the shrinage target

    # what they call "c"
    gamma <- norm(cov - prior, type = "F") ^ 2 # Frobenius norm

    # compute the shrinkage constant
    kappa <- phi / gamma
    shrinkage <- max(0 , min(1, kappa /  t))

  # or use pre-specified number
  } else {

    shrinkage <- shrink

  }

  # compute the shrinkage estimator
  sigma <- shrinkage * prior + (1 - shrinkage) * cov

  # output
  if (portfolio) sigma else list(sigma = sigma, shrinkage = shrinkage)

}
