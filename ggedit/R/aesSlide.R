#' @export
aesSlide=function(type){
  list(type=sliderInput,
       args=list(inputId = paste0('pop',toupper(type)),
                 label = type,
                 min = slideDefaults[[type]][1],
                 max = slideDefaults[[type]][2],
                 value = NA)
  )
}