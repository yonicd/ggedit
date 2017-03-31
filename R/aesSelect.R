#' @title aesSelect
#' @description SelectizeInput UI production for palletes.
#' @param type character of label and inputId of element
#' @return UI object
#' @import shiny
#' @keywords internal
aesSelect=function(type) {
  list(type=shiny::selectizeInput,
       args=list(options=list(plugins=list('drag_drop','remove_button')),
                 inputId = paste0('pop',toupper(type)),
                 label = type,
                 choices=c(scales::hue_pal()(10),NA),selected=NA)
  )
}