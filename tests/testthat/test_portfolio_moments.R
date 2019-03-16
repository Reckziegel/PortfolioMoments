library(PortfolioMoments)

context("testing portfolio_moments()")
data(edhec)

data <- edhec[ , 1:2]

mu_and_sigma <- portfolio_moments(
  R     = data,
  mu    = ~ stats::arima(x = ., order = c(1L, 0L, 0L)),
  sigma = cov_shrink_to_honey
)


test_that("Returns list with two elements: mu and sigma", {

  # class
  expect_is(mu_and_sigma, "list")
  expect_match(class(mu_and_sigma[["mu"]]), "numeric")
  expect_match(class(mu_and_sigma[["sigma"]]), "matrix")
  expect_equal(names(mu_and_sigma)[[1]], "mu")
  expect_equal(names(mu_and_sigma)[[2]], "sigma")

  # names are keeped
  expect_equal(names(mu_and_sigma[["mu"]]), colnames(data))
  expect_equal(colnames(mu_and_sigma[["sigma"]]), colnames(data))
  expect_equal(rownames(mu_and_sigma[["sigma"]]), colnames(data))

  # size
  expect_equal(length(mu_and_sigma[["mu"]]), ncol(data))
  expect_equal(nrow(mu_and_sigma[["sigma"]]), ncol(data))
  expect_equal(ncol(mu_and_sigma[["sigma"]]), ncol(data))
  expect_equal(length(mu_and_sigma), 2)
  expect_equal(length(mu_and_sigma[["mu"]]), 2)
  expect_equal(length(mu_and_sigma[["sigma"]]), 4)

})
