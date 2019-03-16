library(PortfolioMoments)

context("testing cov_ewma()")

stocks <- xts::xts(
  cbind(
    X = rnorm(10, 0, 1),
    Y = rnorm(10, 0, 2),
    Z = rnorm(10, 0, 4)
  ),
  order.by = as.Date('2019-01-01') + 0:9
)

cov_stocks <- cov_ewma(stocks)

test_that("Returns a 3 x 3 covariance matrix with the correct class, names and size", {

  # class
  expect_is(cov_stocks, "matrix")
  expect_match(purrr::map_chr(cov_stocks, class), "numeric")

  # names are keeped
  expect_equal(colnames(cov_stocks), colnames(stocks))
  expect_equal(rownames(cov_stocks), colnames(stocks))

  # size
  expect_equal(ncol(cov_stocks), 3)
  expect_equal(nrow(cov_stocks), 3)
  expect_equal(length(cov_stocks), 9)

})


