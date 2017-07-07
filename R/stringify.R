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
#'  
#'  #add theme change
#'  p<-pList$pointSmooth+theme(panel.background = element_rect(fill='green'))
#'  this.gg<-dput.ggedit(p) #dput the plot
#'  writeLines(this.gg) #show the output
#'  eval(parse(text=this.gg)) #recompile the plot
#'  
#' @rdname dput.ggedit
#' @export 

dput.ggedit<-function(obj,file=""){
  
  objs<-c(cloneRoot(obj,verbose=TRUE),
          sapply(obj$layers,cloneLayer,verbose=TRUE,showDefaults = FALSE),
          cloneFacet(obj$facet,verbose=TRUE))
  
  if(inherits(obj$theme,'theme')) objs<-c(objs,compare(obj$theme,theme_get()))
  
  str.out<-paste0(objs,collapse='+')  
  
  if(nchar(file)>0){
    cat(str.out,file = file) 
  }else{
    str.out
  }
}
