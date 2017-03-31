#' @title is.NullOb
#' @description Checks if any columns have a null in them.
#' @param x vector or data.frame
#' @return logical
#' @keywords internal
is.NullOb <- function(x){
  if(!(is.function(x))) is.null(x) | all(sapply(x, is.null)) else FALSE
} 