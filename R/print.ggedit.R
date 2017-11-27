#' @title Print ggedit objects
#' @description Plots lists of ggplot2 plot objects
#' layout functionality.
#' @param x list of ggplot2 plot objects
#' @param layout matrix, layout of plots like in \code{\link[graphics]{layout}}, 
#' if NULL then a default layout is used, Default: NULL
#' @param byrow boolean, argument passed to default layout (when layout is NULL), used to 
#' transpose the output.
#' @param ... not used
#' @examples
#' p <- as.gglist(pList[1:2])
#' p
#'
#' p1 <- p+geom_hline(aes(yintercept=3))
#' p1
#'
#' print(p1,byrow=TRUE)
#'
#' print(p1,layout = matrix(c(2,2,NA,1),ncol=2))
#'
#' @export
print.ggedit <- function(x, layout=NULL, byrow = FALSE, ...) {

  if (!is.null(x$UpdatedPlots)) {
    x <- x$UpdatedPlots
  }

  if (is.null(layout)) {

    numPlots <- length(x)

    cols <- min(numPlots, 2)
    rows <- ceiling(numPlots / cols)
    
    plot_vec <- 1:numPlots
    
    if(cols*rows>numPlots){
      plot_vec <- c(plot_vec,rep(NA,(cols*rows)-numPlots))
    }
    
    layout <- matrix(plot_vec,ncol=cols,nrow=rows,byrow = byrow)

    if(byrow&rows==1)
      layout <- matrix(plot_vec,ncol=rows,nrow=cols)  
  }
  
    grid::grid.newpage()

    grid::pushViewport(
        grid::viewport(
          layout = grid::grid.layout(nrow(layout), ncol(layout))
        )
      )

    for (i in 1:max(layout,na.rm = TRUE)) {
        thisdim <- which(layout==i,arr.ind = TRUE)
        print(x[[i]], vp = vplayout(thisdim[,1], thisdim[,2]))  
    }
  
}
