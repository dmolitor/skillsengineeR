#' Pull the full list of occupations
#' 
#' @param token Authorization token obtained from \code{get_access_token}
#' @param handle_status How to handle bad HTTP status. Set as either \code{warn} or \code{error}
#' @param response_raw Logical value whether to return the API response as
#'   raw, unparsed text. Defaults to \code{FALSE}
#' @return A data.frame; occupations relevant to query
#' @export
occupations <- function(token, 
                        handle_status = "warn",
                        response_raw = FALSE){
  assert(handle_status %in% c("warn", "error"),
         "Set handle_status as either 'warn' or 'error'")
  assert(response_raw %in% c(TRUE, FALSE),
         "Set response_raw as either TRUE or FALSE")
  req <- httr::GET(url = "https://api.skillsengine.com/v2/occupations",
                   httr::add_headers("Authorization" = paste("bearer",
                                                             token)))
  if (handle_status == "warn") {
    httr::warn_for_status(req)
  } else {
    httr::stop_for_status(req)
  }
  content <- httr::content(req, as = "text")
  if (response_raw) return(content)
  content_structured <- jsonlite::fromJSON(content, flatten = TRUE)
  return(content_structured$result$occupations)
}

#' Provide economic data related to user-provided SOC codes (by state)
#' 
#' @param token Authorization token obtained from \code{get_access_token}
#' @param soc_codes Character vector of SOC codes to extract data for
#' @param state_code State code string to get regional results (Optional)
#' @param handle_status How to handle bad HTTP status. Set as either \code{warn} or \code{error}
#' @param response_raw Logical value whether to return the API response as
#'   raw, unparsed text. Defaults to \code{FALSE}
#' @return A data.frame; occupations relevant to query
#' @examples
#' \dontrun{
#' tkn <- get_access_token(client_id, client_secret)
#' o <- occupations_soc_codes(soc_codes = c("13-2011","15-2031","15-2011"),
#'                            state_code = "TX",
#'                            token = tkn)
#' }
#' @export
occupations_soc_codes <- function(token,
                                  soc_codes,
                                  state_code = NULL,
                                  handle_status = "warn",
                                  response_raw = FALSE) {
  assert(handle_status %in% c("warn", "error"),
         "Set handle_status as either 'warn' or 'error'")
  assert(response_raw %in% c(TRUE, FALSE),
         "Set response_raw as either TRUE or FALSE")
  soc_codes <- paste0(soc_codes,
                      collapse = ",")
  query <- list(soc_codes = soc_codes)
  if (!is.null(state_code)) {
    query <- append(query, list(state_code = state_code))
  }
  req <- httr::GET("https://api.skillsengine.com/v2/occupations/soc_codes",
                   query = query,
                   httr::add_headers('Authorization' = paste("bearer", 
                                                             token)))
  if (handle_status == "warn") {
    httr::warn_for_status(req)
  } else {
    httr::stop_for_status(req)
  }
  content <- httr::content(req, as = "text")
  if (response_raw) return(content)
  content_structured <- jsonlite::fromJSON(content, flatten = TRUE)
  return(content_structured$result$occupations)
}
