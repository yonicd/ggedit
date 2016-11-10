#' @title Creates an independent copy of a ggplot object layer
#' @export
#' @description 
#' Creates copies of ggplot layers from within ggplot objects
#'  that are independent of the parent object.
#' @details ggplot objects are comprimsed of layer objects. Once compiled they 
#' are part of the plot object environment and if they are changed internally regardless of 
#' where they are in the (ie different environment) it will change the original plot. This function allows to 
#' create replicates of the plot layers and edit them independent of the original plot.
#' @param l ggplot2 object layer
#' @return ggproto object
#' @examples
#' p=ggplot(iris,aes(x =Sepal.Length,y=Sepal.Width))
#' p=p+geom_point(aes(colour=Species))+geom_line()
#' p$layers[[1]]
#' newLayer=cloneLayer(l=p$layers[[1]])
#' all.equal(p$layers[[1]],newLayer)

cloneLayer=function(l){
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
  do.call(layer,x)
}