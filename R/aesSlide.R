#' @title aesSlide
#' @description sliderInput UI production for continuous variables
#' @param type character of label and inputId of element
#' @return UI object
#' @export
#' @import shiny
#' @keywords internal
aesSlide=function(type){
  list(type=shiny::sliderInput,
       args=list(inputId = paste0('pop',toupper(type)),
                 label = type,
                 min = ggedit::slideDefaults[[type]][1],
                 max = ggedit::slideDefaults[[type]][2],
                 value = NA)
  )
}