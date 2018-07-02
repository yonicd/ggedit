#' @title print.gglist
#' @description Print method for gglist objects.
#' @export
#' @keywords internal
#' @import ggplot2
#' @importFrom purrr map
print.gglist <- function(x, ...) purrr::map(x, print.ggplot, ...)

print.ggplot <- function(x, newpage = is.null(vp), vp = NULL, ...) {

  set_last_plot(x)
  if (newpage) {
    grid.newpage()
  }
  grDevices::recordGraphics(
    requireNamespace("ggplot2", quietly = TRUE),
    list(), getNamespace("ggplot2")
  )
  data <- ggplot_build(x)
  gtable <- ggplot_gtable(data)
  if (is.null(vp)) {
    grid.draw(gtable)
  }
  else {
    if (is.character(vp)) {
      seekViewport(vp)
    } else {
      pushViewport(vp)
    }
    grid.draw(gtable)
    upViewport()
  }
  invisible(data)
}
environment(print.ggplot) <- asNamespace("ggplot2")

set_last_plot <- function(value) .store$set(value)
environment(set_last_plot) <- asNamespace("ggplot2")
