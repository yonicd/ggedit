#' @title geom_list
#' @description Returns character vector of geom types in ggplot.
#' @param p ggplot
#' @return character vector
#' @export
#' @keywords internal
geom_list <- function(p) {
  g <- unlist(sapply(p$layers, function(x) gsub("Geom", "", proto_features(x)[2])))


  g.list <- sapply(unique(g), function(x) paste0(g[g == x], seq(1, table(g)[[x]])))

  unlist(g.list)
}
