context("get functions")

test_that("get functions are working properly", {
  skip("No API Keys to run test")
  client_id <- get_id_secret()[[1]]
  client_secret <- get_id_secret()[[2]]
  token <- get_access_token(client_id, client_secret)

  expect_is(occupations(token), "data.frame")

  expect_is(occupations_soc_codes(soc_codes = c("13-2011","15-2031","15-2011"),
                                  state_code = "TX",
                                  token = token),
            "data.frame")
})