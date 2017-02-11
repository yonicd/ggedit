#' @export
print.gglist = function(x, ...) l_ply(x, ggplot2:::print.ggplot, ...)