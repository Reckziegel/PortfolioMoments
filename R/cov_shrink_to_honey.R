#' Shrink the Covariance Matrix towards Hollywood!
#'
#' Implements the covariance matrix described in Ledoit & Wold (2003).
#'
#' @param R A vector, matrix, data.frame, xts, timeSeries or zoo object to be checked and coerced.
#' @param shrink A number between 0 and 1.
#' @param portfolio If \code{TRUE} only the covariance matrix is printed. If \code{FALSE} a list is returned with the covariance-matrix and the shrinkage intensity.
#'
#' @return A variance-covariance matrix.
#'
#' @author Bernardo Reckziegel
#'
#' @references Ledoit, Olivier, and Michael Wolf. \emph{Honey, I shrunk the sample covariance matrix.} UPF economics and business working paper 691 (2003).
#'
#' @export
#'
#' @examples
#' library(PortfolioAnalytics)
#' data(edhec)
#' cov_shrink_to_honey(edhec)
#' cov_shrink_to_honey(edhec, portfolio = FALSE)
cov_shrink_to_honey <- function(R, shrink = NULL, portfolio = TRUE) {

  n  <- nrow(R)
  p  <- ncol(R)
  mu <- apply(R, 2, mean)
  R  <- R - matrix(rep(mu, times = n), ncol = p, byrow = TRUE)

  # Covariancia amostral usando  (R - mean)
  sample <- (1 / n) * (t(R) %*% R)

  # Prior
  var     <- matrix(diag(sample), ncol = 1)
  sqrtvar <- sqrt(var)
  tmpMat  <- matrix(rep(sqrtvar, times = p), nrow = p)
  rBar    <- (sum(sum(sample / (tmpMat * t(tmpMat)))) - p) / (p * (p - 1))
  prior   <- rBar * tmpMat * t(tmpMat)
  diag(prior) <- var

  if (!is.null(colnames(R))) {

    colnames(prior) <- colnames(R)
    rownames(prior) <- colnames(R)

  }

  if (is.null(shrink)) {

    # pi-hat
    y      <- R ^ 2
    phiMat <- t(y) %*% y / n - 2 * (t(R) %*% R) * sample / n + sample ^ 2
    phi    <- sum(phiMat)

    # Wrho-hat
    aux1 <- (t(R ^ 3) %*% R) / n
    help <- t(R) %*% R / n
    helpDiag <- matrix(diag(help), ncol = 1)
    aux2 <- matrix(rep(helpDiag, times = p), ncol = p) * sample
    aux3 <- help * matrix(rep(var, times = p), ncol = p)
    aux4 <- matrix(rep(var, times = p), ncol = p) * sample
    thetaMat <- aux1 - aux2 - aux3 + aux4
    diag(thetaMat) <- 0
    rho <- sum(diag(phiMat)) + rBar * sum(sum(((1 / sqrtvar) %*% t(sqrtvar)) * thetaMat))

    # gamma-hat
    gamma <- norm(sample - prior, "F") ^ 2

    # Shrinkage constant
    kappa <- (phi - rho) / gamma
    shrinkage <- max(0, min(1, kappa / n))

  } else {

    shrinkage <- shrink


  }

  # Estimador
  sigma <- shrinkage * prior + (1 - shrinkage) * sample

  # output
  if (portfolio) {
    out <- sigma
  } else {
    out <- list(cov = sigma, prior = prior, shrinkage = shrinkage)
  }

  return(out)

}
