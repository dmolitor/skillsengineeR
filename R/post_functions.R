#' Pull from Competencies API endpoint.
#' 
#' @param text String. Sentence or text entry used to pull competencies.
#' @param token String. Authorization token.
#' @param bias Numeric. Level of bias.
#' @param exclude Character vector. Vector of result elements to omit.
#' @param shush Logical. Print query status.
#' @examples
#' \dontrun{
#' txt <- "i am a welder and used welding equipment and acetylene torches"
#' c <- competencies(text = txt, bias = 0.4)
#' }
#' @export
competencies <- function(text,
                         token,
                         bias = NULL,
                         exclude = NULL,
                         shush = FALSE){
  elements <- c("metadata",
                "occupations",
                "general_work_activities",
                "intermediate_work_activities",
                "detailed_work_activities",
                "workplace_essentials",
                "knowledges",
                "knowledge_subdomains",
                "skills",
                "abilities",
                "tools")
  assert(is.null(exclude) | all(exclude %in% elements),
         c("At least one of 'exclude' is not valid. ",
           "Valid options are as follows:\n\t",
           paste0(elements, collapse = "\n\t")))
  body <- list(text = text)
  if(!is.null(bias)) {body <- append(body, list(bias = bias))}
  if(!is.null(exclude)) {body <- append(body, list(exclude = exclude))}
  post <- httr::POST("https://api.skillsengine.com/v2/competencies/analyze",
                     body = body,
                     httr::add_headers('Authorization' = paste("bearer", 
                                                               token)))
  if (shush == FALSE) {
    if (post$status_code == 200) {
      cat("Valid request\n")
    } else {
      cat(paste0("Bad request. Status: ", post$status_code, "\n"))
    }
  }
  post_content <- httr::content(post, as = "text")
  post_structured <- jsonlite::fromJSON(post_content, flatten = TRUE)
  out <- post_structured$result$competencies_analysis
  return(out)
}

#' Pull from Competencies Flatten API endpoint.
#' 
#' @param text String. Sentence or text entry used to pull competencies.
#' @param token String. Authorization token.
#' @param bias Numeric. Level of bias.
#' @param exclude Character vector. Vector of result elements to omit.
#' @param shush Logical. Print query status.
#' @examples
#' \dontrun{
#' txt <- "i am a welder and used welding equipment and acetylene torches"
#' c <- competencies_flatten(text = txt, bias = 0.4)
#' }
#' @export
competencies_flatten <- function(text,
                                 token,
                                 bias = NULL,
                                 exclude = NULL,
                                 shush = FALSE){
  elements <- c("metadata",
                "occupations",
                "general_work_activities",
                "intermediate_work_activities",
                "detailed_work_activities",
                "workplace_essentials",
                "knowledges",
                "knowledge_subdomains",
                "skills",
                "abilities",
                "tools")
  assert(is.null(exclude) | all(exclude %in% elements),
         c("At least one of 'exclude' is not valid. ",
           "Valid options are as follows:\n\t",
           paste0(elements, collapse = "\n\t")))
  body <- list(text = text)
  if(!is.null(bias)) {body <- append(body, list(bias = bias))}
  if(!is.null(exclude)) {body <- append(body, list(exclude = exclude))}
  post <- httr::POST("https://api.skillsengine.com/v2/competencies/analyze/flatten",
                     body = body,
                     httr::add_headers('Authorization' = paste("bearer", 
                                                               token)))
  if (shush == FALSE) {
    if (post$status_code == 200) {
      cat("Valid request\n")
    } else {
      cat(paste0("Bad request. Status: ", post$status_code, "\n"))
    }
  }
  post_content <- httr::content(post, as = "text")
  post_structured <- jsonlite::fromJSON(post_content, flatten = TRUE)
  out <- post_structured$result$competencies_analysis
  return(out)
}

#' Pull from Competencies Military API endpoint.
#' 
#' @param moc String or numeric. MOC to return competencies for.
#' @param service_branch String. Branch of Armed Forces.
#' @param mpc String.
#' @param exclude Character vector. Vector of return elements to exclude.
#' @param start_date String. Start date of military service.
#' @param end_date String. End date of military service.
#' @param moc_status String. Status of MOC.
#' @param token String. Authorization token.
#' @param shush Logical. Print query status.
#' @examples
#' \dontrun{
#' c <- competencies_military(moc = "2A554E")
#' }
#' @export
competencies_military <- function(moc,
                                  service_branch = NULL,
                                  mpc = NULL,
                                  exclude = NULL,
                                  start_date = NULL,
                                  end_date = NULL,
                                  moc_status = NULL,
                                  token,
                                  shush = FALSE) {
  branches <- c("navy", 
                "army", 
                "coast guard", 
                "marine corps", 
                "air force", 
                "special forces")
  assert(is.null(service_branch) | all(service_branch %in% branches),
         c("At least one of 'service_branch' is not valid. ",
           "Valid options are as follows:\n\t",
           paste0(branches, collapse = "\n\t")))
  mpcs <- c("officer", 
            "warrant", 
            "enlisted")
  assert(is.null(mpc) | all(mpc %in% mpcs),
         c("At least one of 'mpc' is not valid. ",
           "Valid options are as follows:\n\t",
           paste0(mpcs, collapse = "\n\t")))
  elements <- c("metadata",
                "occupations",
                "general_work_activities",
                "intermediate_work_activities",
                "detailed_work_activities",
                "workplace_essentials",
                "knowledges",
                "knowledge_subdomains",
                "skills",
                "abilities",
                "tools")
  assert(is.null(exclude) | all(exclude %in% elements),
         c("At least one of 'exclude' is not valid. ",
           "Valid options are as follows:\n\t",
           paste0(elements, collapse = "\n\t")))
  statuses <- c("current_only", 
                "obsolete_only", 
                "all")
  assert(is.null(moc_status) | all(moc_status %in% statuses),
         c("At least one of 'moc_status' is not valid. ",
           "Valid options are as follows:\n\t",
           paste0(statuses, collapse = "\n\t")))
  body <- list(moc = moc)
  if(!is.null(service_branch)) {
    body <- append(body, list(service_branch = service_branch))
  }
  if(!is.null(mpc)) {body <- append(body, list(mpc = mpc))}
  if(!is.null(exclude)) {body <- append(body, list(exclude = exclude))}
  if(!is.null(start_date)) {body <- append(body, list(start_date = start_date))}
  if(!is.null(end_date)) {body <- append(body, list(end_date = end_date))}
  if(!is.null(moc_status)) {body <- append(body, list(moc_status = moc_status))}
  post <- httr::POST("https://api.skillsengine.com/v2/competencies/analyze/military",
                     body = body,
                     httr::add_headers('Authorization' = paste("bearer", 
                                                               token)))
  if (shush == FALSE) {
    if (post$status_code == 200) {
      cat("Valid request\n")
    } else {
      cat(paste0("Bad request. Status: ", post$status_code, "\n"))
    }
  }
  post_content <- httr::content(post, as = "text")
  post_structured <- jsonlite::fromJSON(post_content, flatten = TRUE)
  out <- post_structured$result$competencies_analysis
  return(out)
}

#' Pull from Skills API endpoint.
#' 
#' @param text String. Sentence or text entry used to pull competencies.
#' @param depth Numeric. Algorithm depth.
#' @param cutoff Numeric. Cutoff value.
#' @param socs Character vector. Vector of SOCs to fine-tune search.
#' @param similarity_scoring String. Whether or not to use similarity scoring.
#' @param token String. Authorization token.
#' @param shush Logical. Print query status.
#' @examples 
#' \dontrun{
#' txt <- "Provide substance abuse education and counseling for at-risk individuals."
#' s <- skills(text = txt,
#'             depth = 20,
#'             cutoff = 5,
#'             similarity_scoring = "true")
#' }
#' @export
skills <- function(text,
                   depth = NULL,
                   cutoff = NULL,
                   socs = NULL,
                   similarity_scoring = NULL,
                   token,
                   shush = FALSE) {
  assert(is.null(similarity_scoring) | 
           all(similarity_scoring %in% c('true', 'false')),
         c("Entry for 'similarity_scoring' is not valid. ",
           "Valid options are as follows:\n\t",
           paste0(c('true', 'false'), collapse = "\n\t")))
  body <- list(text = text)
  if(!is.null(depth)) {body <- append(body, list(depth = depth))}
  if(!is.null(cutoff)) {body <- append(body, list(cutoff = cutoff))}
  if(!is.null(socs)) {
    socs <- paste0(socs, collapse = ",")
    body <- append(body, list(socs = socs))
  }
  if(!is.null(similarity_scoring)) {
    body <- append(body, list(similarity_scoring = similarity_scoring))
  }
  post <- httr::POST("https://api.skillsengine.com/v2/skills/match",
                     body = body,
                     httr::add_headers('Authorization' = paste("bearer", 
                                                               token)))
  if (shush == FALSE) {
    if (post$status_code == 200) {
      cat("Valid request\n")
    } else {
      cat(paste0("Bad request. Status: ", post$status_code, "\n"))
    }
  }
  post_content <- httr::content(post, as = "text")
  post_structured <- jsonlite::fromJSON(post_content, flatten = TRUE)
  out <- post_structured$result
  return(out)
}

#' Pull from Skills Multi-Match API endpoint.
#' 
#' @param sentences Character vector. Sentences used to pull competencies.
#' @param depth Numeric. Algorithm depth.
#' @param cutoff Numeric. Cutoff value.
#' @param socs Character vector. Vector of SOCs to fine-tune search.
#' @param similarity_scoring String. Whether or not to use similarity scoring.
#' @param token String. Authorization token.
#' @param shush Logical. Print query status.
#' @examples 
#' \dontrun{
#' txts <- c("Weld things together", "Use an acetylene torch", "Hammer nails into wood")
#' s <- skills_multi_match(sentences = txts,
#'                         depth = 20,
#'                         cutoff = 5,
#'                         socs = c("17-2141.00",
#'                                  "49-9041.00",
#'                                  "49-1011.00",
#'                                  "49-9071.00"))
#' }
#' @export
skills_multi_match <- function(sentences,
                               depth = NULL,
                               cutoff = NULL,
                               socs = NULL,
                               similarity_scoring = NULL,
                               token,
                               shush = FALSE) {
  assert(is.null(similarity_scoring) |
           all(similarity_scoring %in% c('true', 'false')),
         c("Entry for 'similarity_scoring' is not valid. ",
           "Valid options are as follows:\n\t",
           paste0(c('true', 'false'), collapse = "\n\t")))
  body <- list(sentences = sentences)
  if(!is.null(depth)) {body <- append(body, list(depth = depth))}
  if(!is.null(cutoff)) {body <- append(body, list(cutoff = cutoff))}
  if(!is.null(socs)) {
    socs <- paste0(socs, collapse = ",")
    body <- append(body, list(socs = socs))
  }
  if(!is.null(similarity_scoring)) {
    body <- append(body, list(similarity_scoring = similarity_scoring))
  }
  post <- httr::POST("https://api.skillsengine.com/v2/skills/multi_match",
                     body = body,
                     encode = "json",
                     httr::add_headers('Authorization' = paste("bearer",
                                                               token)))
  if (shush == FALSE) {
    if (post$status_code == 200) {
      cat("Valid request\n")
    } else {
      cat(paste0("Bad request.\n Message: ", httr::content(post), "\n"))
    }
  }
  post_content <- httr::content(post, as = "text")
  post_structured <- jsonlite::fromJSON(post_content, flatten = TRUE)
  out <- post_structured$result$skills
  return(out)
}

#' Pull from Related Topics API endpoint.
#' 
#' @param texts Character Vector. Sentences used to pull related topics.
#' @param socs Character vector. Soc codes to fine-tune related topics.
#' @param token String. Authorization token.
#' @param shush Logical. Print query status.
#' @examples 
#' \dontrun{
#' t <- topics_related(texts = c("ms word", "access", "dexterity"),
#'                     socs = "41-3041.00")
#' }
#' @export
topics_related <- function(texts,
                           socs = NULL,
                           token,
                           shush = FALSE){
  body <- list(texts = texts)
  if(!is.null(socs)) {
    body <- append(body, list(socs = socs))
  }
  post <- httr::POST("https://api.skillsengine.com/v2/topics/related",
                     body = body,
                     encode = "json",
                     httr::add_headers("Authorization" = paste("bearer", 
                                                               token)))
  if (shush == FALSE) {
    if (post$status_code == 200) {
      cat("Valid request\n")
    } else {
      cat(paste0("Bad request. Status: ", post$status_code, "\n"))
    }
  }
  post_content <- httr::content(post, as = "text")
  post_structured <- jsonlite::fromJSON(post_content, flatten = TRUE)
  out <- post_structured$result
  return(out)
}
