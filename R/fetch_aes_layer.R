#' @title fetch_layer_aes
#' @description Checks the aesthetics in a layer for internal use only.
#' @keywords internal
#'
fetch_layer_aes <- function(self, data=data.frame(x = 1), params = list(), stats=list()) {
  missing_aes <- dplyr::setdiff(names(self$default_aes), names(data))

  if (empty(data)) {
    data <- plyr::quickdf(self$default_aes[missing_aes])
  } else {
    data[missing_aes] <- self$default_aes[missing_aes]
  }

  aes_params <- dplyr::intersect(self$aesthetics(), names(params))

  check_aesthetics(params[aes_params], nrow(data))

  data[aes_params] <- params[aes_params]

  data[, -1]
}
