#' @title clone root of ggplot object
#' @description clone root of ggplot object and return either a new gg object or 
#' a string to parse and evaluate
#' @param obj ggplot object
#' @param verbose boolean, toggles to return object or script (TRUE), Default: FALSE
#' @return gg object or script
#' @examples 
#'  cloneRoot(pList$pointSmooth)
#'  cloneRoot(pList$pointSmooth,verbose=TRUE)
#' @seealso 
#'  \code{\link[utils]{capture.output}}
#' @rdname cloneRoot
#' @export 
#' @importFrom utils capture.output
cloneRoot <- function(obj,verbose=FALSE){

  data = 'NULL'
  
  mapping_str <- 'aes()'
  
  if( !is.null(obj$data) ) 
    data <- paste(utils::capture.output(dput(obj$data)),collapse='\n')

  if( !is.null(obj$mapping) ){
    
    mapping <- unlist(obj$mapping)
    
    mapping_str <- sprintf('aes(%s)',paste0(sapply(names(mapping),function(y) paste(y,mapping[[y]],sep='=')),collapse=','))
    
  } 
  
  strout <- sprintf('ggplot(mapping=%s,data=%s)',mapping_str,data)
  
  if( verbose ){
    
    strout
    
  }else{
    
    eval(parse(text=strout))
    
  } 
  
}