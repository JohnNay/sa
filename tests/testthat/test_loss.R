library(sa)
context("Test loss function")

test_that("Testing Bernoulli log-likelihood computation by comparing to R's stats::glm().", {
  dat <- data.frame(my.decision = c(1,0,1,1,1))
  g1 <- stats::glm(my.decision ~ 1, family = binomial,
                   data = dat)
  expect_equal(round(2 * compute_log_lik(as.numeric(predict(g1, dat, type="response")),
                                      dat$my.decision)),  round(g1$deviance))
})

