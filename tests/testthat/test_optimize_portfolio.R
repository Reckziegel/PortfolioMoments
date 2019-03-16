library(PortfolioAnalytics)
library(PortfolioMoments)
library(dplyr)

context("testing optimize_portfolio()")
data(edhec)

data <- edhec[ , 1:2]

mean_var_spec <- PortfolioAnalytics::portfolio.spec(assets = colnames(data)) %>%
  PortfolioAnalytics::add.constraint(type = "box", min = 0.00, max = 1.00) %>%
  PortfolioAnalytics::add.objective(type = "risk", name = "var") %>%
  PortfolioAnalytics::add.objective(type = "return", name = "mean")

opt <- optimize_portfolio(
  R               = data,
  portfolio       = mean_var_spec,
  optimize_method = 'random',
  search_size     = 500,
  mu              = forecast::ets,
  sigma           = cov_ewma
)

mu_fcast <- purrr::map(as.list(data), forecast::ets) %>%
  purrr::map(forecast::forecast, h = 1) %>%
  purrr::map_dbl(purrr::chuck, "mean")
opt_weights <- PortfolioAnalytics::extractWeights(opt)
projected_mu <- mu_fcast %*% opt_weights
projected_sigma <- t(opt_weights) %*% cov_ewma(data) %*% opt_weights


test_that("Returns list with the optimal portfolio", {

  # class
  expect_is(opt, "optimize.portfolio")

  # Are projected returns and portfolio returns equal?
  # If yes, we know for sure that "mu" was indeed used in the optimization.
  expect_equal(
    as.vector(opt$objective_measures$mean),
    as.vector(projected_mu),
    tolerance = 0.001
    )

  # Is the squared root of projected sigma and portfolio standard deviation equal?
  # If yes, we know for sure that "sigma" was indeed used in the optimization.
  expect_equal(
    as.vector(opt$objective_measures$StdDev),
    as.vector(sqrt(projected_sigma)),
    tolerance = 0.001
  )

})
