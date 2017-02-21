#' @export
aesColourContNS=function(type,session) {
  ns<-session$ns
  id=gsub("-a", "", ns("a"))
  iId=paste0('pop',toupper(type),'fixedPal')
  fP=ns(paste0('pop',toupper(type),'fixedPal'))
  #column(width=2,
  div(
  selectizeInput(inputId = fP,
                 label='Pallete',
                 choices = c('Manual','Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges', 'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds', 'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd'),
                 selected = 'Blues'),
  
  conditionalPanel(paste0("input['", id, "-", iId ,"'] == Manual"),
    lapply(c('Low','High'),function(x,type){
    if(x=='Low'){
      pad='padding:0px 0px 0px 10px;'
      init.col='red'
    } 
    if(x=='High'){
      pad='padding:0px 10px 0px 0px;'
      init.col='blue'
    } 
    
    column(width=6, style=pad,
           do.call(what = 'colourInput',
                   args=list(inputId = ns(paste0('pop',toupper(type),x)),
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

