context("authorization functions")

test_that("authorization functions work", {
  expect_null(set_id_secret())
  expect_invisible(set_id_secret())
  
  skip("No API keys to run test")
  expect_is(get_id_secret(), "character")
  expect_is(get_access_token(), "character")
})