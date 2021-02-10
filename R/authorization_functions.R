#' Set SkillsEngine Client ID and Client Secret
#' 
#' @return No value; called for side effects
#' @export
set_id_secret <- function(){
  message(
    c("RStudio will open your .Renviron file in a new tab. ",
      "Enter your Client ID and Client Secret as described below.\n",
      "Using the following naming convention is necessary for 'get_id_secret()'",
      " to work:\n",
      "\tSKILLSENGINE_CLIENT_ID=<client id>\n",
      "\tSKILLSENGINE_CLIENT_SECRET=<client secret>\n")
  )
  usethis::edit_r_environ()
  message("\nRestart your R Session!")
}

#' Get SkillsEngine Client ID and Client Secret
#'
#' @return List of length 2; Contains Client ID and Secret keys
#' @export
get_id_secret <- function(){
  client_id <- Sys.getenv("SKILLSENGINE_CLIENT_ID")
  client_secret <- Sys.getenv("SKILLSENGINE_CLIENT_SECRET")
  assert(client_id != "" & client_secret != "",
         c("Environment variables ",
           "'SKILLSENGINE_CLIENT_ID' and 'SKILLSENGINE_CLIENT_SECRET'",
           " do not exist.\nTo initialize these ",
           "credentials, call 'set_id_secret()'."))
  return(list(id = client_id, secret = client_secret))
  
}

#' Get SkillsEngine Access Token
#'
#' @param client_id Client ID provided by \code{get_id_secret}, or manually entered
#' @param client_secret Client Secret provided by \code{get_id_secret}, or manually entered
#' @param handle_status How to handle bad HTTP status. Set as either \code{warn} or \code{error}
#' @return A string; the Access Token
#' @export
get_access_token <- function(client_id,
                             client_secret,
                             handle_status = "warn"){
  assert(handle_status %in% c("warn", "error"),
         "Set handle_status as either 'warn' or 'error'")
  post <- httr::POST(url = "https://api.skillsengine.com/api/token",
                     body = list("grant_type" = "client_credentials",
                                 "client_id" = client_id,
                                 "client_secret" = client_secret),
                     encode = "json")
  if (handle_status == "warn") {
    httr::warn_for_status(post)
  } else {
    httr::stop_for_status(post)
  }
  access_token <- httr::content(post)$access_token
  return(access_token)
}
