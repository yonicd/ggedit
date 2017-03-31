#' @title aesSelectNS
#' @description SelectizeInput UI production for palletes for module.
#' @param type character of label and inputId of element
#' @param session shiny session object
#' @return UI object
#' @import shiny
#' @keywords internal
aesSelectNS=function(type,session) {
  ns<-session$ns
  list(type=shiny::selectizeInput,
       args=list(options=list(plugins=list('drag_drop','remove_button')),
                 inputId = ns(paste0('pop',toupper(type))),
                 label = type,
                 choices=c(scales::hue_pal()(10),NA),selected=NA)
  )
}