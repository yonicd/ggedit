#' @title print.gglist
#' @description Print method for gglist objects.
#' @export
#' @keywords internal
#' @import ggplot2
#' @importFrom plyr l_ply ldply ddply quickdf
#' 
print.gglist = function(x, ...) plyr::l_ply(x, ggplot2:::print.ggplot, ...)