context("check api")

test_that("check api functions are operational", {
  skip("No API Keys to run test")
  client_id <- get_id_secret()[[1]]
  client_secret <- get_id_secret()[[2]]
  token <- get_access_token(client_id, client_secret)

  expect_is(test_access(token), "character")

  expect_is(freshness(token), "character")
})