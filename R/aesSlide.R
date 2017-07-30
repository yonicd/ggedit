#' @title aesSlide
#' @description sliderInput UI production for continuous variables
#' @param type character of label and inputId of element
#' @return UI object
#' @import shiny
#' @keywords internal
aesSlide=function(type){
  
  slideDefaults <- ggedit_opts$get('slideDefaults')
  
  list(type=shiny::sliderInput,
       args=list(inputId = paste0('pop',toupper(type)),
                 label = type,
                 min = slideDefaults[[type]][1],
                 max = slideDefaults[[type]][2],
                 value = NA)
  )
}