context("authorization functions")

test_that("authorization functions work", {
  expect_null(set_id_secret())
  expect_invisible(set_id_secret())
})