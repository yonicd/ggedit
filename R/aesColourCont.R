#' @title aesColorCont
#' @description ColorInput UI production for continuous variables.
#' @param type character of label and inputId of element
#' @return UI object
#' @keywords internal
#' @import shiny
aesColourCont=function(type) {
  #column(width=2,
  shiny::div(
    shiny::selectizeInput(inputId = paste0('pop',toupper(type),'fixedPal'),
                 label='Pallete',
                 choices = c('Manual','Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges', 'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds', 'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd'),
                 selected = 'Blues'),
    shiny::conditionalPanel(paste0('input[',paste0('pop',toupper(type),'fixedPal'),']=="Manual"'),
    lapply(c('Low','High'),function(x,type){
    if(x=='Low'){
      pad='padding:0px 0px 0px 10px;'
      init.col='red'
    } 
    if(x=='High'){
      pad='padding:0px 10px 0px 0px;'
      init.col='blue'
    } 
    
    shiny::column(width=6, style=pad,
           do.call(colourpicker::colourInput,
                   args=list(inputId = paste0('pop',toupper(type),x),
                             label = x,
                             value =  init.col,
                             returnName = F,
                             showColour = "background")
                  )
           )
  },type)
  )
  )
}

