#' Pull the full list of occupations
#' 
#' @param token String. Authorization token.
#' @param shush Logical. Whether to print request status.
#' @export
occupations <- function(token, shush = FALSE){
  req <- httr::GET(url = "https://api.skillsengine.com/v2/occupations",
                   httr::add_headers("Authorization" = paste("bearer",
                                                             token)))
  if (shush == FALSE) {
    if (req$status_code == 200) {
      cat("Valid request\n")
    } else {
      cat(paste0("Bad request. Status: ", req$status_code, "\n"))
    }
  }
  content <- httr::content(req, as = "text")
  content_structured <- jsonlite::fromJSON(content, flatten = TRUE)
  return(content_structured$result$occupations)
}

#' Provide economic data related to user-provided SOC codes (by state).
#' 
#' @param token String. Authorization token.
#' @param soc_codes Character vector. SOC codes to pull data for.
#' @param state_code String. State code to get more specific results. (Optional)
#' @param shush Logical. Whether to print request status.
#' @examples
#' \dontrun{
#' o <- occupations_soc_codes(soc_codes = c("13-2011","15-2031","15-2011"),
#'                            state_code = "TX")
#' }
#' @export
occupations_soc_codes <- function(token,
                                  soc_codes,
                                  state_code = NULL,
                                  shush = FALSE) {
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
  if (shush == FALSE) {
    if (req$status_code == 200) {
      cat("Valid request\n")
    } else {
      cat(paste0("Bad request. Status: ", req$status_code, "\n"))
    }
  }
  content <- httr::content(req, as = "text")
  content_structured <- jsonlite::fromJSON(content, flatten = TRUE)
  return(content_structured$result$occupations)
}