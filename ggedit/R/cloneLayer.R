#' @title Creates an independent copy of a ggplot layer object
#' @export
#' @description 
#' Creates copies of ggplot layers from within ggplot objects
#'  that are independent of the parent object.
#' @details ggplot objects are comprimsed of layer objects. Once compiled they 
#' are part of the plot object environment and if they are changed internally regardless of 
#' where they are in the (ie different environment) it will change the original plot. This function allows to 
#' create replicates of the plot layers and edit them independent of the original plot. When setting verbose to TRUE
#' function returns the ggplot2 call as a string to paste in regular ggplot script to generate the layer.
#' @param l ggplot2 object layer
#' @param verbose toggle to control if the output is ggproto object (verbose==FALSE,default) or string of layer call (verbose==TRUE)
#' @return ggproto or string object (conditional on verbose)
#' @examples
#' p=ggplot(iris,aes(x =Sepal.Length,y=Sepal.Width))
#' p=p+geom_point(aes(colour=Species))+geom_line()
#' p$layers[[1]]
#' newLayer=cloneLayer(l=p$layers[[1]])
#' all.equal(p$layers[[1]],newLayer)
#' (v=cloneLayer(l=p$layers[[1]],verbose=T))
#' eval(parse(text=v))
#' all.equal(p$layers[[1]],eval(parse(text=v)))

cloneLayer=function(l,verbose=F){
  layer.names=c('mapping','data','geom','position',
                'stat','show.legend','inherit.aes',
                'aes_params','geom_params','stat_params')
  x=sapply(layer.names,function(y){
    b=l[[y]]
    
    if('waiver'%in%class(b)) b=NULL
    
    if(y=='geom') b=eval(parse(text=class(b)[1]))
    
    if(y%in%c('position','stat')) {
      b=gsub(y, "", tolower(class(b)[1]))
    }
    
    b
  }) 
  x$params=append(x$stat_params,x$geom_params)
  x$params=append(x$params,x$aes_params)
  x$params=x$params[!duplicated(names(x$params))]
  x$geom_params<-x$aes_params<-x$stat_params<-NULL

  if(verbose){
    nm=names(x)
    nm=nm[!nm%in%c('geom','params','mapping')]
    paste0(paste0('geom_',tolower(gsub('Geom','',class(x$geom)[1]))),'(',
           paste0('mapping=aes(',paste0(lapply(names(x$mapping),function(item){paste(item,x$mapping[[item]],sep="=")}),collapse=","),')'),',',
           paste0(lapply(names(unlist(x$params)),function(item) {
             if(item=='formula'){
               paste0('formula=as.formula("',paste0(as.character(x$params[[item]])[-1],collapse='~'),'")')
             }else{
               paste(item,x$params[[item]],sep="=") 
             }
             }),collapse=","),',',
           paste0(lapply(nm,function(y){
             if(is.logical(x[[y]])) out=paste(y,x[[y]],sep="=")
             if(is.character(x[[y]])) out=paste(y,paste0('"',x[[y]],'"'),sep="=")
             if(is.null(x[[y]])) out=paste(y,'NULL',sep="=")
             return(out)
           }),collapse=','),
           ')')
  }else{
    do.call(layer,x) 
  }
}