context("authorization functions")

test_that("authorization functions work", {
  expect_null(set_id_secret())
  expect_invisible(set_id_secret())
  
  skip("No API Keys to run test")
  expect_is(get_id_secret(), "list")

  client_id <- get_id_secret()[[1]]
  client_secret <- get_id_secret()[[2]]
  expect_is(get_access_token(client_id, client_secret), "character")
})