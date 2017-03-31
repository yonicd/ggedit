#' @title as.gglist
#' @description Creates structure of lists of ggplots.
#' @param p gg
#' @return gg
#' @export
#' @keywords internal
as.gglist=function(p) structure(p, class=c("gglist", "ggplot"))