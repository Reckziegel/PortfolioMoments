library(PortfolioMoments)

context("testing mu_ewma()")

stocks <- xts::xts(
  cbind(
    X = rnorm(10, 0, 1),
    Y = rnorm(10, 0, 2),
    Z = rnorm(10, 0, 4)
  ),
  order.by = as.Date('2019-01-01') + 0:9
)

mu <- mu_ewma(stocks)

test_that("Returns a 3 x 3 covariance matrix with the correct class, names and size", {

  # class
  expect_is(mu, "numeric")
  expect_match(purrr::map_chr(mu, class), "numeric")

  # names are keeped
  expect_equal(names(mu), colnames(stocks))

  # size
  expect_equal(length(mu), 3)

})
