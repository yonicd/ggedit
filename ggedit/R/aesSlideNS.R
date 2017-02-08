#' @export
aesSlideNS=function(type,session){
  ns<-session$ns
  list(type=sliderInput,
       args=list(inputId = ns(paste0('pop',toupper(type))),
                 label = type,
                 min = slideDefaults[[type]][1],
                 max = slideDefaults[[type]][2],
                 value = NA)
  )
}