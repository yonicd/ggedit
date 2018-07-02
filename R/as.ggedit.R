#' @title Is the object of class ggedit
#' @export
#' @description Is the object of class ggedit. Very basic for many functions
#'  in the package.
#' @param x an object
#' @return logical - is the object of class ggedit
is.ggedit <- function(x) inherits(x, 'ggedit')

#' @title Try to coerce a ggplot object into a ggedit object
#' @export
#' @description Applied to ggplot objects to use the ploting function of ggedit.
#' @param plot an object
#' @return an object of class ggedit
#' @seealso
#' \code{\link{print.ggedit}}, \code{\link[ggplot2]{ggplot}}
#' @examples
#' 
#' p  <- ggplot2::ggplot(iris,ggplot2::aes(x =Sepal.Length,y=Sepal.Width))
#' 
#' p1 <- p + 
#' ggplot2::geom_point(ggplot2::aes(colour=Species)) + 
#' ggplot2::geom_line()
#' 
#' p2 <- p + 
#' ggplot2::geom_point() + 
#' ggplot2::geom_smooth(method='loess')
#' 
#' p3 <- list(p1,p2)
#' 
#' p4 <- as.ggedit(p3)
#' 
#' p4
as.ggedit <- function(plot) {
  UseMethod('as.ggedit')
}

#' @export
as.ggedit.ggedit <- function(plot) {
  plot
}

#' @export
as.ggedit.list <- function(plot) {
  structure(plot, class = c("ggedit", "gglist"))
}

#' @export
as.ggedit.ggplot <- function(plot) {
  plot <- list(plot)
  structure(plot, class = c("ggedit", "gglist","gg","ggplot"))
}

