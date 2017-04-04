#' @title aesSlideNS
#' @description sliderInput UI production for continuous variables for module
#' @param type character of label and inputId of element
#' @param session shiny session object
#' @return UI object
#' @import shiny
#' @keywords internal
aesSlideNS=function(type,session){
  ns<-session$ns
  list(type=shiny::sliderInput,
       args=list(inputId = ns(paste0('pop',toupper(type))),
                 label = type,
                 min = ggedit::slideDefaults[[type]][1],
                 max = ggedit::slideDefaults[[type]][2],
                 value = NA)
  )
}