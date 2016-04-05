library(sa)
context("Test SA functions")

testthat::test_that("sobol_sa() returns correct object", {
  fake_abm <- function(params, out) {
    x1 <- params[1]
    x2 <- params[2]
    if (out=="sq") return(x1^2 + x2 + rnorm(1, 0))
    if (out=="ident") return(x1 + x2 + rnorm(1, 0))
  }
  inputs <- lapply(list(param1 = NA, param2 = NA), 
                   function(x) list(random_function = "qunif",
                                    ARGS = list(min = 0, max = 1)))
  testthat::expect_is(sobol_sa(fake_abm, inputs, "sq")@result, "sobol2007")
})

testthat::test_that("pc_sa() returns correct object", {
  # Unconstrained Analysis
  fake_abm <- function(params, out) {
    x1 <- params[1]
    x2 <- params[2]
    if (out=="sq") return(x1^2 + x2 + rnorm(1, 0))
    if (out=="ident") return(x1 + x2 + rnorm(1, 0))
  }
  inputs <- lapply(list(param1 = NA, param2 = NA),
                   function(x) list(random_function = "qunif",
                                    ARGS = list(min = 0, max = 1)))
  
  testthat::expect_is(pc_sa(fake_abm, inputs, "sq")@result, "src")
  testthat::expect_is(pc_sa(fake_abm, inputs, "sq", method = "pcc")@result, "pcc")
})

