#' @title aesColor
#' @description ColorInput UI production for discrete variables.
#' @param type character of label and inputId of element
#' @return UI object
#' @keywords internal
aesColour=function(type) {
  list(type=colourpicker::colourInput,
       args=list(inputId = paste0('pop',toupper(type)),
                 label = type,
                 value = NA,
                 allowTransparent = T)
  )
}