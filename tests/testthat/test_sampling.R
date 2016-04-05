library(sa)
context("Test sampling helper functions")

test_that("keep_satisfied() returns correct type of object", {
  fake_constraints <- "param1 < 0.5 & param2 > 0.5"
  fake_data <- data.frame(param1 = runif(100), param2 = runif(100))
  fake_constraints <- with(fake_data, eval(parse(text=fake_constraints)))
  res <- keep_satisfied(fake_data, fake_constraints)
  
  expect_is(res, "data.frame")
})

