context("post functions")

test_that("post functions are working properly", {
  skip("No API Keys to run test")
  client_id <- get_id_secret()[[1]]
  client_secret <- get_id_secret()[[2]]
  token <- get_access_token(client_id, client_secret)
  
  # Test competencies
  txt <- "i am a welder and used welding equipment and acetylene torches"
  test <- competencies(text = txt,
                       bias = 0.4,
                       token = token)
  expect_type(test,
              "list")
  expect_equal(length(test), 9)

  # Test competencies_flatten
  test <- competencies_flatten(text = txt,
                               bias = 0.4,
                               token = token)
  expect_type(test,
              "list")
  expect_equal(length(test), 11)

  # Test competencies_military
  test <- competencies_military(moc = "2A554E",
                                token = token)
  expect_type(test,
              "list")
  expect_equal(length(test), 12)

  # Test skills
  txt <- "Provide substance abuse education and counseling for at-risk individuals."
  test <- skills(text = txt,
                 depth = 20,
                 cutoff = 5,
                 similarity_scoring = "true",
                 token = token)
  expect_type(test,
              "list")
  expect_equal(length(test), 2)

  # Test skills_multi_match
  txts <- c("Weld things together",
            "Use an acetylene torch",
            "Hammer nails into wood")
  test <- skills_multi_match(sentences = txts,
                             depth = 20,
                             cutoff = 5,
                             socs = c("17-2141.00",
                                      "49-9041.00",
                                      "49-1011.00",
                                      "49-9071.00"),
                             token = token)
  expect_type(test,
              "list")
  expect_equal(length(test), 3)

  # Test topics_related
  test <- topics_related(texts = c("ms word", "access", "dexterity"),
                         socs = "41-3041.00",
                         token = token)
  expect_type(test,
              "list")
  expect_equal(length(test), 2)
})