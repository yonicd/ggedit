#' @title aesColor
#' @description ColorInput UI production for discrete variables.
#' @param type character of label and inputId of element
#' @param session shiny session object
#' @return UI object
#' @keywords internal
aesColour <- function(type, session = NULL) {
  if (is.null(session)) {
    ns <- function(x) x
  } else {
    ns <- session$ns
  }

  list(
    type = colourpicker::colourInput,
    args = list(
      inputId = ns(paste0("pop", toupper(type))),
      label = type,
      value = NA,
      allowTransparent = TRUE
    )
  )
}
