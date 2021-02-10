#' Test connection to SkillsEngine API
#'
#' @param token Authorization token obtained from \code{get_access_token}
#' @param handle_status How to handle bad HTTP status. Set as either \code{warn} or \code{error}
#' @return No return value; Prints status to console
#' @export
test_access <- function(token, handle_status = "warn"){
  assert(handle_status %in% c("warn", "error"),
         "Set handle_status as either 'warn' or 'error'")
  post <- httr::GET(url = "https://api.skillsengine.com/api/test", 
                    httr::add_headers('Authorization' = paste("bearer", 
                                                              token)))
  if (handle_status == "warn") {
    httr::warn_for_status(post)
  } else {
    httr::stop_for_status(post)
  }
  print(httr::content(post)$message)
}

#' Pull the freshness tag of the data
#' 
#' @param token Authorization token obtained from \code{get_access_token}
#' @param handle_status How to handle bad HTTP status. Set as either \code{warn} or \code{error}
#' @return A string; the data freshness tag
#' @export
freshness <- function(token, handle_status = "warn"){
  assert(handle_status %in% c("warn", "error"),
         "Set handle_status as either 'warn' or 'error'")
  req <- httr::GET(url = "https://api.skillsengine.com/v2/skills/freshness",
                   httr::add_headers("Authorization" = paste("bearer",
                                                             token)))
  if (handle_status == "warn") {
    httr::warn_for_status(req)
  } else {
    httr::stop_for_status(req)
  }
  content <- jsonlite::fromJSON(httr::content(req, as = "text"), 
                                flatten = T)
  return(content$result$freshness_tag)
}
