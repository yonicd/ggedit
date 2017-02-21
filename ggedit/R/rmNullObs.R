#' @export
#' @keywords internal
rmNullObs <- function(x) {
  if(!(is.function(x))) {
    x = x[!(sapply(x, is.NullOb))]
    lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
  }
}