library(PortfolioMoments)

context("testing cov_shrink_to_market()")

stocks <- xts::xts(
  cbind(
    X = rnorm(10, 0, 1),
    Y = rnorm(10, 0, 2),
    Z = rnorm(10, 0, 4)
  ),
  order.by = as.Date('2019-01-01') + 0:9
)

sigma <- cov_shrink_to_market(stocks)

test_that("Returns a 3 x 3 covariance matrix with the correct class, names and size", {

  # class
  expect_is(sigma, "matrix")
  expect_match(purrr::map_chr(sigma, class), "numeric")

  # names are keeped
  expect_equal(colnames(sigma), colnames(stocks))
  expect_equal(rownames(sigma), colnames(stocks))

  # size
  expect_equal(ncol(sigma), 3)
  expect_equal(nrow(sigma), 3)
  expect_equal(length(sigma), 9)

})
