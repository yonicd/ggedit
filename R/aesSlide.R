#' @title aesSlide
#' @description sliderInput UI production for continuous variables
#' @param type character of label and inputId of element
#' @param session shiny session object
#' @return UI object
#' @import shiny
#' @keywords internal
aesSlide <- function(type, session = NULL) {
  if (is.null(session)) {
    ns <- function(x) x
  } else {
    ns <- session$ns
  }

  slideDefaults <- ggedit_opts$get("slideDefaults")
  
  list(
    type = shiny::sliderInput,
    args = list(
      inputId = ns(paste0("pop", toupper(type))),
      label = type,
      min = slideDefaults[[type]][1],
      max = slideDefaults[[type]][2],
      value = NA_real_
    )
  )
}
