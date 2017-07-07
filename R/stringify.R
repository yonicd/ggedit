#' @title Convert ggplot object to a string call
#' @description Convert ggplot object to a string call
#' @param obj compiled ggplot object
#' @param file either a character string naming a file or a \link{connection}. " " indicates output to the console, Default: " "
#' @return character
#' @examples 
#'  pList$pointSmooth #original compiled plot
#'  this.gg<-dput.ggedit(pList$pointSmooth) #dput the plot
#'  writeLines(this.gg) #show the output
#'  eval(parse(text=this.gg)) #recompile the plot
#' @rdname stringify.plot
#' @export 

dput.ggedit<-function(obj,file=""){
  str.out<-paste0(c(cloneRoot(obj,verbose=TRUE),
           sapply(obj$layers,cloneLayer,verbose=TRUE,showDefaults = FALSE),
           cloneFacet(obj$facet,verbose=TRUE)),collapse='+')  
  
  if(nchar(file)>0){
    cat(str.out,file = file) 
  }else{
    str.out
  }
}
