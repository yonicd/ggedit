closestColHex <- function(target, n = 3, superset = grDevices::colours(distinct = TRUE)) {
  target <- as.numeric(grDevices::col2rgb(target))

  dist_mat <- grDevices::col2rgb(superset) - target

  dist <- apply(dist_mat, 2, function(x) sum(abs(x)))

  closest <- order(dist)[1:n]

  superset[closest]
}
