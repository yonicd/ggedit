#' @title gggsave
#' @description Wrapper of ggsave that saves ggplot or list of ggplot 
#' objects to image or pdf.
#' @param filename a character string giving the name of the file. 
#' If it is of the form "|cmd", the output is piped to the command given by cmd. 
#' If it is NULL, then no external file is created (effectively, no drawing occurs),
#' but the device may still be queried (e.g., for size of text). For use with onefile = FALSE 
#' give a C integer format such as "Rplot\%03d.pdf" (the default in that case)
#' @param plot ggplot or list of ggplots to save, defaults to last plot displayed
#' @param ... other arguments passed on to graphics device
#' @details default output is to create one pdf regardless of size of list of plots inputted
#' @examples 
#' \dontrun{gggsave(pList)} 
#' @return nothing
#' @export
gggsave <- function(filename="Rplot.pdf",plot=last_plot(),...){
  cl=class(plot)
  if(any(c('gg','ggplot')%in%cl)) pList=list(plot)
  if('list'%in%cl) pList=plot
  if(!all(sapply(pList,function(x) any(c('gg','ggplot')%in%class(x))))) 
    stop('plot argument not a ggplot or list of ggplots')
  
  l=list(...)
  l$plot=as.gglist(pList)
  if(!'path'%in%names(l)) l$path=getwd()
  fileSplit=unlist(strsplit(filename,'[.]'))
  l$filename=fileSplit[1]
  
  if(!'device'%in%names(l)){
    if(length(fileSplit)==2){
      l$device=fileSplit[2]  
    }else{
      l$device='pdf'  
    } 
  } 
  if(l$device=='pdf'){
    if(!'onefile'%in%names(l)) l$onefile=TRUE
  }else{
    if(length(l$plot)==1) l$onefile=TRUE else l$onefile=FALSE
  }
  
  if(!dir.exists(l$path)){
    warning('creating figure directory',call. = FALSE)
    dir.create(l$path)
  }
  
  if(l$onefile) l$filename=paste(l$filename,l$device,sep='.') else l$filename=paste0(l$filename,"%03d.",l$device)
  if(l$device!='pdf') l$onefile<-NULL

  do.call('ggsave',l)
}