aesSelect=function(type) {
  list(type=selectizeInput,
       args=list(options=list(plugins=list('drag_drop','remove_button')),
                 inputId = paste0('pop',toupper(type)),
                 label = type,
                 choices=c(hue_pal()(10),NA),selected=NA)
  )
}