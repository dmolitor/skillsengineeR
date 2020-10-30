#' Set SkillsEngine Client ID and Client Secret
#'
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
#' @param client_id String. Provided by \code{get_id_secret}
#' @param client_secret String. Provided by \code{get_id_secret}
#' @param shush Logical. Print query status or not
#' @export
get_access_token <- function(client_id,
                             client_secret,
                             shush = FALSE){
  post <- httr::POST(url = "https://api.skillsengine.com/api/token",
                     body = list("grant_type" = "client_credentials",
                                 "client_id" = client_id,
                                 "client_secret" = client_secret))
  if (shush == FALSE) {
    if (post$status_code == 200) {
      cat("Valid request\n")
    } else {
      cat(paste0("Bad request. Status: ", post$status_code, "\n"))
    }
  }
  access_token <- httr::content(post)$access_token
  return(access_token)
}