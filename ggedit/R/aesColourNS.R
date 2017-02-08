#' @export
aesColourNS=function(type,session) {
  ns<-session$ns
  list(type=colourInput,
       args=list(inputId = ns(paste0('pop',toupper(type))),
                 label = type,
                 value = NA,
                 allowTransparent = T)
  )
}