#' @title Plots ggedit plot output objects
#' @description Plots ggplot2 plots edited by ggedit with 
#' layout functionality. 
#' @param x ggedit output of list of plots from the ggedit output
#' @param ... list which defines the location of each plot within
#' the viewport layout. (see details)
#' @details If ... is NULL then a default layout is used filling in
#' a grid.layout row.wise. Defining plot.layout as a nested list giving 
#' the rows and the column indicies of each plot will produce a specific layout.
#' @examples
#' \donttest{
#' p=ggplot(iris,aes(x =Sepal.Length,y=Sepal.Width))
#' p=p+geom_point(aes(colour=Species))+geom_line()
#' p
#' x=ggedit(p)
#' plot(x)
#' plot(x$UpdatedPlots)
#' p1=pList[c(1:2)]
#' x1=ggedit(p1)
#' plot(x1)
#' plot(x1,plot.layout = list(list(rows=2,cols=2),list(rows=1,cols=1:2)))
#' }
#' @export
#' 
plot.ggedit=function(x,...){
  plot.layout=NULL
  l<-list(...)
  list2env(l,envir = environment())
  if(!is.null(x$UpdatedPlots)) x=x$UpdatedPlots
  if(is.null(plot.layout)){
    plot.layout=1:length(x)
    numPlots = length(x)
    cols=min(numPlots,2)
    plotCols = cols                      
    plotRows = ceiling(numPlots/plotCols)
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(plotRows,plotCols)))
    for (i in 1:numPlots) {
      curRow = ceiling(i/plotCols)
      curCol = (i-1) %% plotCols + 1
      print(x[[i]], vp = vplayout(curRow, curCol))
    }
  }else{
    numPlots = length(x)
    plotRows=max(unlist(lapply(plot.layout,'[',1)))
    plotCols=max(unlist(lapply(plot.layout,'[',2)))
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(plotRows,plotCols)))
    for (i in 1:numPlots) {
      print(x[[i]], vp = vplayout(plot.layout[[i]]$rows, plot.layout[[i]]$cols))
    }
    }
}