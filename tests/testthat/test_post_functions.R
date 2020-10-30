context("post functions")

test_that("post functions are working properly", {
  client_id <- get_id_secret()[[1]]
  client_secret <- get_id_secret()[[2]]
  token <- get_access_token(client_id, client_secret)
  
  txt <- "i am a welder and used welding equipment and acetylene torches"
  expect_output(str(competencies(text = txt, 
                                 bias = 0.4,
                                 token = token)), 
                "Valid request\nList of 9")
  
  expect_output(str(competencies_flatten(text = txt, bias = 0.4,
                                         token = token)), 
                "Valid request\nList of 11")
  
  expect_output(str(competencies_military(moc = "2A554E",
                                          token = token)),
                "Valid request\nList of 12")
  
  txt <- "Provide substance abuse education and counseling for at-risk individuals."
  expect_output(str(skills(text = txt,
                           depth = 20,
                           cutoff = 5,
                           similarity_scoring = "true",
                           token = token)),
                "Valid request\nList of 2")
  
  txts <- c("Weld things together", 
            "Use an acetylene torch", 
            "Hammer nails into wood")
  expect_output(str(skills_multi_match(sentences = txts,
                                       depth = 20,
                                       cutoff = 5,
                                       socs = c("17-2141.00",
                                                "49-9041.00",
                                                "49-1011.00",
                                                "49-9071.00"),
                                       token = token)),
                "Valid request\nList of 3")
  
  expect_output(str(topics_related(texts = c("ms word", "access", "dexterity"),                   
                                   socs = "41-3041.00",
                                   token = token)),
                "Valid request\nList of 2")
})