context("authorization functions")

test_that("authorization functions work", {
  expect_null(set_id_secret())
  expect_invisible(set_id_secret())
  
  skip("No API keys to run test")
  client <- get_id_secret()
  expect_type(client, "list")
  expect_equal(length(client), 2)
  client_id <- get_id_secret()[[1]]
  client_secret <- get_id_secret()[[2]]
  expect_is(get_access_token(client_id, client_secret), "character")
})