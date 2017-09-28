#' @title aesColorCont
#' @description ColorInput UI production for continuous variables.
#' @param type character of label and inputId of element
#' @param session shiny session object
#' @return UI object
#' @keywords internal
#' @import shiny
aesColourCont=function( type, session = NULL ) {

  if(is.null(session)){
    ns <- function(x) x 
  }else{
    ns <- session$ns  
  }
  
  id  <- gsub("-a", "", ns("a"))
  iId <- sprintf('pop%sfixedPal',toupper(type))
  fP  <- ns(iId)
  
  shiny::div(
    
    shiny::selectizeInput( 
      inputId  = fP,
      label    = 'Pallete',
      choices  = c('Manual','Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges', 'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds', 'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd'),
      selected = 'Blues'),
    
    shiny::conditionalPanel(
      sprintf("input['%s-%s'] == 'Manual'",id,iId),
                            
      lapply(c('Low','High'),
           function(x,type){
                            if( x=='Low' ){
                              pad <- 'padding:0px 0px 0px 10px;'
                              init.col <- 'red'
                            } 
                            if( x=='High' ){
                              pad<- 'padding:0px 10px 0px 0px;'
                              init.col <- 'blue'
                            } 
    
                            shiny::column(
                              width=6, 
                              style=pad,
                              do.call(colourpicker::colourInput,
                                      args=list(inputId = ns(sprintf('pop%s%s',toupper(type),x)),
                                                label = x,
                                                value =  init.col,
                                                returnName = FALSE,
                                                showColour = "background")
                                          )
                                   )
                          },
           type)
    )
  )
  
}
