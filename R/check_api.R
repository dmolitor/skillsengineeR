#' Test connection to SkillsEngine API
#'
#' @param token String. Authorization token.
#' @export
test_access <- function(token){
  post <- httr::GET(url = "https://api.skillsengine.com/api/test", 
                    httr::add_headers('Authorization' = paste("bearer", 
                                                              token)))
  print(httr::content(post)$message)
}

#' Pull the freshness tag of the data
#' 
#' @param token String. Authorization token.
#' @param shush Logical. Whether to print request status.
#' @export
freshness <- function(token, shush = FALSE){
  req <- httr::GET(url = "https://api.skillsengine.com/v2/skills/freshness",
                   httr::add_headers("Authorization" = paste("bearer",
                                                             token)))
  if (shush == FALSE) {
    if (req$status_code == 200) {
      cat("Valid request\n")
    } else {
      cat(paste0("Bad request. Status:", req$status_code, "\n"))
    }
  }
  content <- jsonlite::fromJSON(httr::content(req, as = "text"), 
                                flatten = T)
  return(content$result$freshness_tag)
}