assert <- function(condition,
                   message = NULL) {
  if(is.na(condition)) {
    stop("Condition is NA", call. = FALSE)
  }
  if(!condition) {
    if(is.null(message)) {
      stop(FALSE, call. = FALSE)
    } else {
      stop(message, call. = FALSE)
    }
  }
}
