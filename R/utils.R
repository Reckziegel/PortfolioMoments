# repmat <- function(x, m, n) {
#
#   # check the class
#   if (is.matrix(x)) {
#
#     mx <- dim(x)[1]
#     nx <- dim(x)[2]
#
#   } else if (is.atomic(x)) {
#
#     mx <- 1
#     nx <- length(x)
#
#   } else {
#
#     stop('x must be numeric or a matrix.')
#
#   }
#
#   matrix(data  = matrix(data = x, nrow = mx, ncol = nx * n),
#          nrow  = mx * m,
#          ncol  = nx * n,
#          byrow = TRUE)
#
# }
#
#
# squeeze <- function (x)  {
#
#   # The same as the squeeze function in MATLAB
#   d <- dim(x)
#   dim(x) <- d[d > 1]
#   x
#
# }


# .validate_mu <- function(mu) {
#
#   .mu_funs <- c("arfima", "Arima", "auto.arima", "ets", "baggedModel",
#                 "baggedETS", "bats", "tbats", "nnetar", "tslm")
#
#   .fun_name <- lazyeval::expr_text(mu) %>%
#     stringr::str_remove_all(., "~") %>%
#     stringr::str_remove_all(., pattern = '\\(.*')
#
#   if (!(.fun_name %in% .mu_funs)) {
#     message("tidymoments currently only support functions from the forecast package.",
#             "\n",
#             "Please, change the argument ", crayon::cyan(.fun_name), " to one of the options bellow:"
#     )
#
#     purrr::iwalk(
#       .x = .mu_funs,
#       .f = ~ cat(.y, ":", .x, "\n")
#     )
#
#     stop()
#
#   }
#
# }

# .validate_sigma <- function(sigma) {
#
#   .sigma_funs <- c("shrink_honey", "shrink_market", "shrink_bayes_stein")
#
#   .fun_name <- lazyeval::expr_text(sigma) %>%
#     stringr::str_remove_all(., "~") %>%
#     stringr::str_remove_all(., pattern = '\\(.*')
#
#   if (!(.fun_name %in% .sigma_funs)) {
#     message("tidymoments currently only support functions XYZ.",
#             "\n",
#             "Please, change the argument ", crayon::cyan(.fun_name), " to one of the options bellow:"
#     )
#
#     purrr::iwalk(
#       .x = .sigma_funs,
#       .f = ~ cat(.y, ":", .x, "\n")
#     )
#
#     stop()
#
#   }
#
# }



# tidy_moments <- function(R, mu, sigma) {
#
#   #.validate_mu(mu)
#   #.validate_sigma(sigma)
#
#   .mu_mapper    <- rlang::as_function(mu)
#   .sigma_mapper <- rlang::as_function(sigma)
#
#   mu <- R %>%
#     as.list() %>%
#     purrr::map(.mu_mapper) %>%
#     purrr::map_dbl(~ purrr::chuck(forecast::forecast(., h = 1), 'mean'))
#
#   sigma <- .sigma_mapper(R)
#
#   list(mu = mu, sigma = sigma)
#
# }



# .validate_mu <- function(mu) {
#
#   .mu_funs <- c("arfima", "Arima", "auto.arima", "ets", "baggedModel",
#                "baggedETS", "bats", "tbats", "nnetar", "tslm")
#
#   .fun_name <- lazyeval::expr_text(mu) %>%
#     stringr::str_remove_all("~") %>%
#     str_remove_all(pattern = '\\(.*')
#
#   if (!(.fun_name %in% .mu_funs)) {
#     message("tidymoments currently only support functions from the forecast package.",
#             "\n",
#             "Please, change the argument ", crayon::cyan(.fun_name), " to one of the options bellow:"
#             )
#
#     purrr::iwalk(
#       .x = .mu_funs,
#       .f = ~ cat(.y, ":", .x, "\n")
#     )
#
#     stop()
#
#   }
#
# }
#
#
# .validate_mu(~auto.arima(y = 1))
#




#   -----------------------------------------------------------------------
#
# stationary_bootstrap <- function(data, B, w) {
#
#   # Implements the stationary bootstrap for bootstrapping stationary, dependent series
#
#   # USAGE:
#
#   #   [BSDATA, INDICES] = stationary_bootstrap(DATA, B, W)
#
#   # INPUTS:
#
#   #   DATA   - T by 1 vector of data to be bootstrapped
#
#   #   B      - Number of bootstraps
#
#   #   W      - Average block length. P, the probability of starting a new block is defined P=1/W
#
#   # OUTPUTS:
#
#   #   BSDATA  - T by B matrix of bootstrapped data
#
#   #   INDICES - T by B matrix of locations of the original BSDATA=DATA(indexes);
#
#   # COMMENTS:
#
#   #   To generate bootstrap sequences for other uses, such as bootstrapping vector processes, set DATA to (1:N)'.
#
#
#
#   # Get data dimensions
#
#   t <- nrow(data)
#
#   k <- ncol(data)
#
#
#
#   # Input Checking
#
#   if (nargs() != 3) {
#
#     stop('3 inputs required')
#
#   } else if (k > 1) {
#
#     stop('DATA must be a column vector')
#
#   } else if (t < 2) {
#
#     stop('DATA must have at least 2 observations.')
#
#   } else if (is.vector(w) != TRUE | w <= 0) {
#
#     stop('W must be a positive scalar.')
#
#   } else if (is.vector(B) != TRUE | B < 1 | floor(B) != B) {
#
#     stop('B must be a positive scalar integer')
#
#   }
#
#
#
#   # Define the probability of a new block
#
#   p <- 1 / w
#
#   # Set up the bsdata and indices
#
#   indices <- matrix(data = 0, nrow = t, ncol = B)
#
#   # Initial positions
#
#   indices[1, ] <- ceiling(t * runif(n = B))
#
#   # Set up the random numbers
#
#   select <- matrix(data = runif(n = t * B), nrow = t, ncol = B) < p
#
#   indices[select] <- ceiling(runif(n = sum(sum(select))) * t)
#
#   for (i in 2:t) {
#
#     # Determine whether we stay (rand>p) or move to a new starting value (rand<p)
#
#     indices[i, -select[i, ]] <- indices[i - 1, -select[i, ]] + 1
#
#   }
#
#   indices[indices > t] <- indices[indices > t] - t
#
#   # The indices make finding the bsdata simple
#
#   bsdata <- data[indices]
#
#
#
#   out <- list(indices = indices, bsdata = bsdata)
#
#   return(out)
#
#
#
# }
#
#
# #   -----------------------------------------------------------------------
#
# model_confidence_set <- function(losses, alpha, B, w, boot = c('stationary', 'block')) {
#
#   # Compute the model confidence set of Hansen, Lunde and Nason
#
#   # USAGE:
#
#   #   [INCLUDEDR] = mcs(LOSSES,ALPHA,B,W)
#
#   #   [INCLUDEDR,PVALSR,EXCLUDEDR,INCLUDEDSQ,PVALSSQ,EXCLUDEDSQ] = mcs(LOSSES,ALPHA,B,W,BOOT)
#
#   # INPUTS:
#
#   #   LOSSES     - T by K matrix of losses
#
#   #   ALPHA      - The final pval to use in the MCS
#
#   #   B          - Number of bootstrap replications
#
#   #   W          - Desired block length
#
#   #   BOOT       - [OPTIONAL] 'STATIONARY' or 'BLOCK'.  Stationary will be used as default.
#
#   # OUTPUTS:
#
#   #   INCLUDEDR  - Included models using R method
#
#   #   PVALSR     - Pvals using R method
#
#   #   EXCLUDEDR  - Excluded models using R method
#
#   #   INCLUDEDSQ - Included models using SQ method
#
#   #   PVALSSQ    - Pvals using SQ method
#
#   #   EXCLUDEDSQ - Excluded models using SQ method
#
#   # COMMENTS:
#
#   #   This version of the MCS operates on quantities that should be "bads", such as losses.  If the
#
#   #   quantities of interest are "goods", such as returns, simply call MCS with -1*LOSSES
#
#   # EXAMPLES
#
#   #   MCS with 5# size, 1000 bootstrap replications and an average block length of 12
#
#   #       losses = bsxfun(@plus,chi2rnd(5,[1000 10]),linspace(.1,1,10));
#
#   #       [includedR, pvalsR] = mcs(losses, .05, 1000, 12)
#
#   #   MCS on "goods"
#
#   #       gains = bsxfun(@plus,chi2rnd(5,[1000 10]),linspace(.1,1,10));
#
#   #       [includedR, pvalsR] = mcs(-gains, .05, 1000, 12)
#
#   #   MCS with circular block bootstrap
#
#   #       [includedR, pvalsR] = mcs(losses, .05, 1000, 12, 'BLOCK')
#
#
#
#   # Input Checking
#
#   if (nargs() < 4 | nargs > 5) {
#
#     stop('4 or 5 inputs required')
#
#   } else if (nargs() == 4) {
#
#     boot <- 'stationary'
#
#   } else if (boot != 'stationary' | boot != 'block') {
#
#     stop('BOOT must be either STATIONARY or BLOCK.')
#
#   }
#
#   # Get the length of the data
#
#   t <- nrow(losses)
#
#   # if (t < 2) {
#
#   #     stop('LOSSES must have at least 2 observations.')
#
#   # } else if (is.vector(alpha) == FALSE | alpha >= 1 | alpha <= 0) {
#
#   #     stop('ALPHA must be a scalar between 0 and 1')
#
#   # } else if (is.vector(B) == FALSE | B < 1 | floor(B) != B) {
#
#   #     stop('B must be a positive scalar integer')
#
#   # } else if (is.vector(w) == FALSE | w < 1 | floor(w) != w) {
#
#   #     stop('W must be a positive scalar integer')
#
#   # }
#
#   #
#
#   # 1. Compute the indices to use throughout the proceedure
#
#   if (boot == 'block') {
#
#     bsdata <- block_bootstrap(t(1:t), B, w)
#
#   } else {
#
#     bsdata <- stationary_bootstrap(t(1:t), B, w)
#
#   }
#
#
#
#   # All of these values cab be computed once
#
#   M0 <- ncol(losses)
#
#   # The i,j element contains the l(i,t)-l(j,t)
#
#   dijbar <- matrix(data = 0, nrow = M0, ncol = M0)
#
#   for (j in 1:M0) {
#
#     dijbar[j, ] <- mean(losses - repmat(losses[ , j], 1, M0))
#
#   }
#
#   # for each j, compute dij*-bar using the BSdata, than the compute
#
#   # var(dijbar)
#
#   # 2(a)
#
#   dijbarstar <- array(data = 0, dim = c(M0, M0, B))
#
#   for (b in 1:B) {
#
#     meanworkdata <- mean(losses[bsdata[ , b], ])
#
#     for (j in 1:M0) {
#
#       # The i,j element contains the l(b,i,t)-l(b,j,t)
#
#       dijbarstar[j, ,b] <- meanworkdata - meanworkdata[j]
#
#     }
#
#   }
#
#
#
#   # 2(a)
#
#   #vardijbar <- mean((dijbarstar - repmat(dijbar, [1 1 B]))  ^ 2, 3)
#
#   vardijbar <-  vardijbar + diag(matrix(data = 1, nrow = M0, ncol = 1))
#
#   # 2(b) depends on the det of models under conderations, so it will have to go in the loop
#
#
#
#   # These values are used in the empirical distributions an do not depend on the number of models
#
#   z0 <- (dijbarstar - repmat(dijbar, [1 1 B])) / repmat(sqrt(vardijbar), [1 1 B])
#
#   zdata0 <- dijbar / sqrt(vardijbar)
#
#   # Only these depend on teh set of selected models
#
#   excludedR <- matrix(data = 0, nrow = M0, ncol = 1)
#
#   pvalsR <- matrix(data = 1, nrow = M0, ncol = 1)
#
#   for (i in 1:(M0 - 1)) {
#
#     include <- setdiff(1:M0, excludedR)
#
#     m <- nrow(included)
#
#     z <- z0[included, included, ]
#
#     # Max over the abs value of z in each matrix
#
#     empdistTR <- squeeze(max(max(abs(z),[],1),[],2))
#
#     zdata <- zdata0[included, included]
#
#     TR <- max(max(zdata))
#
#     pvalsR[i] <- mean(empdistTR > TR)
#
#     # Finally compute the model to remove, which depends on the maximum
#
#     # standardized average (among the remaining models)
#
#     # 1. compute dibar
#
#     dibar <- apply(dijbar[included, included], 2, mean) * (m / (m - 1))
#
#     # 2. compute var(dibar)
#
#     dibstar <- squeeze(apply(dijbarstar[included, included, ], 2, mean)) * (m / (m - 1))
#
#     vardi <- mean((t(dibstar) - repmat(dibar, B, 1)) ^ 2)
#
#     t <- dibar / sqrt(vardi)
#
#     # Remove the max of t
#
#     temp <- apply(t, 2, max)
#
#     modeltoremove <- apply(t, 2, which.max)
#
#     excludedR[i] <- included[modeltoremove]
#
#   }
#
#   # The MCS pval is the max up to that point
#
#   maxpval <- pvalsR[1]
#
#   for (i in 2:M0) {
#
#     if (pvalsR[i] < maxpval) {
#
#       pvalsR[i] <- maxpval
#
#     } else {
#
#       maxpval <- pvalsR[i]
#
#     }
#
#   }
#
#   # Add the final remaining model to excluded
#
#   excludedR[length(excludedR)] <- setdiff(1:M0, excludedR)
#
#   # The included models are all of these where the first pval is > alpha
#
#   pl <- find(pvalsR >= alpha, 1, 'first')
#
#   includedR <- excludedR[pl:M0]
#
#   excludedR <- excludedR[1:(pl - 1)]
#
#
#
#   excludedSQ <- matrix(data = 0, nrow = M0, ncol = 1)
#
#   pvalsSQ    <- matrix(data = 1, nrow = M0, ncol = 1)
#
#   for (i in 1:(M0 - 1)) {
#
#     included <- setdiff(1:M0, excludedSQ)
#
#     m <- length(included)
#
#     z <- z0[included, included, ]
#
#     # Only supposed to sub every element once.  Instead sum twice and
#
#     # divide by 2 in each matrix (B of them)
#
#     empdistTSQ <- squeeze(sum(sum(z ^ 2)) / 2)
#
#
#
#     zdata <- zdata0[included, included]
#
#     TSQ <- sum(sum(zdata ^ 2)) / 2
#
#
#
#     pvalsSQ[i] <- mean(empdistTSQ > TSQ)
#
#     # Finally compute the model to remove, which depends on the maximum
#
#     # standardized average (among the remaining models)
#
#     # 1. compute dibar
#
#     dibar <- apply(dijbar[included, included], 2, mean) * (m / (m - 1))
#
#     # 2. compute var(dibar)
#
#     dibstar <- squeeze(apply(dijbarstar[included, included, ], 2, mean)) * (m / (m - 1))
#
#     vardi <- mean((t(dibstar) - repmat(dibar, B, 1)) ^ 2)
#
#     t <- dibar / sqrt(vardi)
#
#     # Remove the max of t
#
#     temp <- max(t)
#
#     modeltoremove <- which.max(t)
#
#     excludedSQ[i] <- included[modeltoremove]
#
#   }
#
#   # The MCS pval is the max up to that point
#
#   maxpval <- pvalsSQ[1]
#
#   for (i in 2:M0) {
#
#     if (pvalsSQ[i] < maxpval) {
#
#       pvalsSQ[i] <- maxpval
#
#     } else {
#
#       maxpval <- pvalsSQ[i]
#
#     }
#
#   }
#
#   # Add the final remaining model to excluded
#
#   excludedSQ[length(excludedSQ)] <- setdiff(1:M0, excludedSQ)
#
#   # The included models are all of these where the first pval is > alpha
#
#   pl <- find(pvalsSQ >= alpha, 1, 'first')
#
#   includedSQ <- excludedSQ(pl:M0)
#
#   excludedSQ <- excludedSQ(1:(pl - 1))
#
#
#
#   # printing the results
#
#   out <- list(includedR = includedR, pvalsR = pvalsR,
#
#               excludedR = excludedR, includedSQ = includedSQ,
#
#               pvalsSQ = pvalsSQ, excludedSQ = excludedSQ)
#
#   return(out)
#
#
#
# }
#
#
# #   -----------------------------------------------------------------------
#
# bsds <- function(bench, models, B, w, type = c('stationary', 'studenized'), boot = c('stationary', 'block')) {
#
#
#
#   # Calculate Whites and Hansens p-vals for out-performance using unmodified data or studentized
#
#   # residuals,  the latter often providing better power, particularly when the losses functions are
#
#   # heteroskedastic
#
#   #
#
#   # USAGE:
#
#   #   [C] = bsds_studentized(BENCH,MODELS,B,W)
#
#   #   [C,U,L] = bsds_studentized(BENCH,MODELS,B,W,TYPE,BOOT)
#
#   #
#
#   # INPUTS:
#
#   #   BENCH  - Losses from the benchmark model
#
#   #   MODELS - Losses from each of the models used for comparison
#
#   #   B      - Number of Bootstrap replications
#
#   #   W      - Desired block length
#
#   #   TYPE   - String, either 'STANDARD' or 'STUDENTIZED'.  'STUDENTIZED' is the default, and
#
#   #              generally leads to better power.
#
#   #   BOOT   - [OPTIONAL] 'STATIONARY' or 'BLOCK'.  Stationary is used as the default.
#
#   #
#
#   # OUTPUTS:
#
#   #   C      - Consistent P-val(Hansen)
#
#   #   U      - Upper P-val(White) (Original RC P-vals)
#
#   #   L      - Lower P-val(Hansen)
#
#   #
#
#   # COMMENTS:
#
#   #   This version of the BSDS operates on quantities that should be 'bads', such as losses.  The null
#
#   #   hypothesis is that the average performance of  the benchmark is as small as the minimum average
#
#   #   performance across the models.  The alternative is that the minimum average loss across the
#
#   #   models is smaller than the the average performance of the benchmark.
#
#   #
#
#   #   If the quantities of interest are 'goods', such as returns, simple call bsds_studentized with -1*BENCH and -1*MODELS
#
#   #
#
#   # EXAMPLES:
#
#   #   Standard Reality Check with 1000 bootstrap replications and a window size of 12
#
#   #       bench = randn(1000,1).^2;
#
#   #       models = randn(1000,100).^2;
#
#   #       [c,realityCheckPval] = bsds(bench, models, 1000, 12)
#
#   #   Standard Reality Check with 1000 bootstrap replications, a window size of 12 and a circular
#
#   #   block bootstrap
#
#   #       [c,realityCheckPval] = bsds(bench, models, 1000, 12, 'BLOCK')
#
#   #   Hansen's P-values
#
#   #       SPAPval = bsds(bench, models, 1000, 12)
#
#   #   Both Pvals on "goods"
#
#   #       bench = .01 + randn(1000,1);
#
#   #       models = randn(1000,100);
#
#   #       [SPAPval,realityCheckPval] = bsds(-bench, -models, 1000, 12)
#
#
#
#   # Input Checking
#
#   if (nargs() < 4 | nargs() > 6) {
#
#     stop('4 to inputs required')
#
#     #} else if (nargs() == 4) {
#
#     #    boot = 'STATIONARY'
#
#     #    type = 'STUDENTIZED';
#
#   } else if (nargs() == 5) {
#
#     boot <- 'STATIONARY'
#
#   }
#
#   # Get the length of the data
#
#   tb <- nrow(bench)
#
#   kb <- ncol(bench)
#
#   if (kb > 1) {
#
#     stop('BENCH must be a column vector')
#
#   } else if (tb < 2) {
#
#     stop('BENCH must have at least 2 observations.')
#
#   }
#
#   t <- nrow(models)
#
#   k <- ncol(models)
#
#   if (t != tb) {
#
#     stop('BENCH and MODELS must have the same number of observations.')
#
#   } else if (is.vector(B) == FALSE | B < 1 | floor(B) != B) {
#
#     stop('B must be a positive scalar integer')
#
#   } else if (is.vector(w) == FALSE | w < 1 | floor(w) != w) {
#
#     stop('W must be a positive scalar integer')
#
#   }
#
#
#
#
#
#   if (boot == 'block') {
#
#     bsdata <- block_bootstrap(t((1:t)), B, w)
#
#   } else {
#
#     bsdata <- stationary_bootstrap(t((1:t)), B, w)
#
#   }
#
#
#
#   # OK now we have the bootstraps, what to do with them?
#
#   diffs <- models - repmat(bench, 1, k)
#
#
#
#   # First compute the boostarap sample averages, db*
#
#   # Second compute the variance estimate, omegak
#
#
#
#   # First the weghts
#
#   q <- 1 / w
#
#   i <- 1:(t - 1)
#
#   kappa <- ((t - i) / t )  * (1 - q) ^ i + i / t * (1 - q) ^ (t - i)
#
#   # Next compute the variances
#
#   vars <- t(matrix(data = 0, nrow = k, ncol = 1))
#
#   for (i in 1:k) {
#
#     workdata <- diffs[ ,i] - mean(diffs[ ,i]) # apply(diffs, 2, mean)
#
#     vars[i] <- t(workdata) %*% workdata / t
#
#     for (j in 1:(t - 1)) {
#
#       vars[i] <- vars[i] + 2 * kappa[j] %*% (t(workdata[1:(t - j)]) %*% workdata[(j + 1):t]) / t
#
#     }
#
#   }
#
#
#
#   # Aold is the original method to compute the truncation point
#
#   Aold <- 1/4 * t ^ (0.25) * sqrt(vars / t)
#
#   #mean(Aold)
#
#   # A new used the log(log(t)) rule
#
#   Anew <- sqrt((vars / t) * 2 * log(log(t)))
#
#
#
#   # Only recenter if the average is reasonably small or the model is better (in which case mean(diffs) is negative).
#
#   # If it is unreasonably large set the mean adjustment to 0
#
#   gc <- mean(diffs) * (mean(diffs) < Anew)
#
#
#
#   # The lower assumes that every loss function that is worse than BM is unimportant for the asymptotic distribution, hence if its mean is
#
#   # less than 0, g=0.  This is different from the consistent where the threshold was it had to be greater than -A(i)
#
#   gl <- pmin(0, mean(diffs))
#
#
#
#   #Then the upper, which assumes all models used are reasonably close to
#
#   #the benchmark that they coudl be better
#
#   gu <- mean(diffs)
#
#
#
#   # Perf will hold the boostrapped statistics for B iterations
#
#   perfc <- matrix(data = 0, nrow = B, ncol = k)
#
#   perfl <- matrix(data = 0, nrow = B, ncol = k)
#
#   perfu <- matrix(data = 0, nrow = B, ncol = k)
#
#   if (type == 'studenized') {
#
#     stdDev <- sqrt(vars)
#
#   } else {
#
#     stdDev <- matrix(data = 1, nrow = 1, ncol = k)
#
#   }
#
#
#
#   for (i in 1:k) {
#
#     workdata <- diffs[ ,i]
#
#     # the i'th column of perf holds the B bootstrapped statistics
#
#     mworkdata  <- mean(workdata(bsdata))
#
#     perfc[ , i] <- t((mworkdata - gc[i])) / stdDev(i)
#
#     perfl[ , i] <- t((mworkdata - gl[i])) / stdDev(i)
#
#     perfu[ , i] <- t((mworkdata - gu[i])) / stdDev(i)
#
#   }
#
#   # Compute the test statistic
#
#   stat  <-  min(mean(diffs) / stdDev)
#
#   # Compute the min in each row
#
#   perfc <- apply(perfc, 1, min)
#
#   perfc <- pmin(perfc,0)
#
#   perfl <- apply(perfl,1, min)
#
#   perfl <- pmin(perfl,0)
#
#   perfu <- apply(perfu, 1, min)
#
#   perfu <- pmin(perfu,0)
#
#   # Count the number of time the min is below the statistic
#
#   c <- mean(perfc < stat)
#
#   l <- mean(perfl < stat)
#
#   u <- mean(perfu < stat)
#
#
#
#   out <- list(c = c, l = l, u = u)
#
#   return(out)
#
#
#
# }
#
#
# #   -----------------------------------------------------------------------
#
# block_bootstrap <- function(data, B, w) {
#
#   # Implements a circular block bootstrap for bootstrapping stationary, dependent series
#
#   # USAGE:
#
#   #   [BSDATA, INDICES]=block_bootstrap(DATA,B,W)
#
#   # INPUTS:
#
#   #   DATA    - T by 1 vector of data to be bootstrapped
#
#   #   B       - Number of bootstraps
#
#   #   W       - Block length
#
#   # OUTPUTS:
#
#   #   BSDATA  - T by B matrix of bootstrapped data
#
#   #   INDICES - T by B matrix of locations of the original BSDATA=DATA(indexes);
#
#   # COMMENTS:
#
#   #   To generate bootstrap sequences for other uses, such as bootstrapping vector processes, set DATA to (1:N)'.
#
#
#
#   # Input Checking
#
#   if (nargs() != 3) {
#
#     stop('3 inputs required')
#
#   }
#
#   # Get the length of the data
#
#   t <- nrow(data)
#
#   k <- ncol(data)
#
#
#
#   if (k > 1) {
#
#     stop('DATA must be a column vector')
#
#   } else if (t < 2) {
#
#     stop('DATA must have at least 2 observations.')
#
#   } else if (is.vector(w) != TRUE | w < 1 | floor(w) != w || w > t) {
#
#     stop('W must be a positive scalar integer smaller than T')
#
#   } else if (is.vector(B) != TRUE | B < 1 | floor(B) != B) {
#
#     stop('B must be a positive scalar integer')
#
#   }
#
#
#
#   # Compute the number of blocks needed
#
#   s <- ceiling(t / w)
#
#   # Generate the starting points
#
#   Bs <- floor(matrix(data = runif(n = s * B), nrow = s, ncol = B) * t) + 1
#
#   indices <- matrix(data = 0, nrow = s * w, ncol = B)
#
#   index <- 1
#
#   # Adder is a variable that needs to be added each loop
#
#   adder <- repmat(t(t(0:(w - 1))), 1, B)
#
#
#
#   for (i in 1:seq(from = 1, to = t, by = w)) {
#
#     indices[i:(i + w - 1), ] <- repmat(Bs[index, ], w, 1) + adder
#
#     index <- index + 1
#
#   }
#
#   indices <- indices[1:t, ]
#
#   indices(indices > t) <- indices(indices > t) - t
#
#   bsdata <- data(indices)
#
#
#
#   out <- list(indices = indices, bsdata = bsdata)
#
#   return(out)
#
#
#
# }
#
#
#
# #   -----------------------------------------------------------------------
#
#
