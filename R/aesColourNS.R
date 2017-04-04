#' @title aesColorNS
#' @description ColorInput UI production for discrete variables for module.
#' @param type character of label and inputId of element
#' @param session shiny session object
#' @return UI object
aesColourNS=function(type,session) {
  ns<-session$ns
  list(type=colourpicker::colourInput,
       args=list(inputId = ns(paste0('pop',toupper(type))),
                 label = type,
                 value = NA,
                 allowTransparent = T)
  )
}