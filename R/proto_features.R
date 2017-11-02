#' @title ggplot2 layer proto extraction
#' @description Extract geom, stat and position protos from a ggplot2 layer
#' @param l ggproto
#' @return data.frame
#' @export
#' @examples
#' proto_features(ggplot2::geom_smooth())
#' proto_features(ggplot2::annotation_logticks())
#'
proto_features <- function(l) {
  a <- sapply(c("position", "geom", "stat"), function(x) {
    class(l[[x]])[1]
  })

  data.frame(t(a), stringsAsFactors = FALSE)
}
