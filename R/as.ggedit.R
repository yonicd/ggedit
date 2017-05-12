#' @title Try to coerce a ggplot object into a ggedit object
#' @export
#' @description Applied to ggplot objects to use the ploting function of ggedit.
#' @param p an object
#' @return an object of class ggedit
#' @seealso 
#' \code{\link{plot.ggedit}},\code{\link[ggplot2]{ggplot}}
#' @examples
#' p=ggplot2::ggplot(iris,ggplot2::aes(x =Sepal.Length,y=Sepal.Width))
#' p1=p+ggplot2::geom_point(ggplot2::aes(colour=Species))+ggplot2::geom_line()
#' p2=p+ggplot2::geom_point()+ggplot2::geom_smooth(method='loess')
#' p3=list(p1,p2)
#' p4=as.ggedit(p3)
#' plot(p4)
as.ggedit<-function(p){
  if(any(c('gg','ggplot','list')%in%class(p))) class(p)<-c('ggedit')
  return(p)
}