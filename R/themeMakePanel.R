#' @title themeMakePanel
#' @description BS modal production for the gg theme.
#' @param x list from themeFetch
#' @param y character, names of nested themes elements, Default: NULL
#' @param session shiny session object
#' @return UI
#' @import shiny
#' @keywords internal
themeMakePanel=function( x, y=NULL, session = NULL ){
  
  if( any(names(x[[1]])=='call') ){
    
    if( is.null(y) ) 
      y <- names(x)
    
    out <- themePanel(x, y, session)
    
  }else{
    
    a <- lapply(names(x[[1]]),
                FUN = function(y)
                  themeMakePanel( 
                    x=x[[1]][y],
                    y=names(x), 
                    session
                    )
                )
    
    out <- shiny::tabPanel(title=names(x),
                           do.call(
                             what = shiny::tabsetPanel,
                             args = unlist(a,recursive = FALSE)
                             )
                           )
    
  }
  
  return(out)
  
}
