#' @export
aesColour=function(type) {
  list(type=colourInput,
       args=list(inputId = paste0('pop',toupper(type)),
                 label = type,
                 value = NA,
                 allowTransparent = T)
  )
}