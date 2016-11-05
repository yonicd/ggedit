remove_geom <- function(p, geom) {
  layers <- lapply(p$layers, function(x) if(any(grepl(paste0('(?i)',geom),class(x$geom)))) NULL else x)
  layers <- layers[!sapply(layers, is.null)]
  
  p$layers <- layers
  p
}