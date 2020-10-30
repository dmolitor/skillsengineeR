context("assert")

test_that("assertions work properly", {
  expect_silent(assert(T == T))
  expect_silent(assert(F == F))
  
  expect_error(assert(T == F))
  expect_error(assert(T == T & T == F))
  
  expect_error(assert(NA == T))
})