#' @title as.gglist
#' @description Creates structure of lists of ggplots.
#' @param p gg
#' @return gg
#' @export
#' @keywords internal
as.gglist <- function(p) {
  cl <- c("ggedit", "gglist", "gg", "ggplot")
  
  if (inherits(p, "ggmatrix")) {
    cl <- c(cl, "ggmatrix")
  }
  
  structure(p, class = cl)
}

#' @export
#' @import ggplot2
#' @importFrom plyr llply
"-.gg" <- function(e1, e2) {
  
  pf <- proto_features(e2)
  
  if (inherits(e1, "gglist")) {
    e3 <- as.gglist(
      plyr::llply(e1, function(t1) {
        geom <- tolower(gsub('^Geom','',pf$geom))
        layers <- t1$layers
        layer.type <- lapply(t1$layers, function(x) class(x$geom))
        a.rm <- which(grepl(paste0("(?i)", geom), layer.type))
        if(length(a.rm)>0){
          remove_geom(t1, geom,max(a.rm)) 
        }else{
          t1
        }
      })
    )
  }else{
    geom <- tolower(gsub('^Geom','',pf$geom))
    layers <- e1$layers
    layer.type <- lapply(e1$layers, function(x) class(x$geom))
    a.rm <- which(grepl(paste0("(?i)", geom), layer.type))
    e3 <- remove_geom(e1, geom, max(a.rm))
  }
  
  attributes(e3) <- attributes(e1)
  
  e3
}

#' @export
#' @importFrom ggplot2 ggplot_add
#' @importFrom plyr llply
"+.gg" <- function(e1, e2) {

  if(inherits(e1,'gglist')){
    
    e3 <- plyr::llply(e1,ggplot_add,object=e2)
    
    attributes(e3) <- attributes(e1)
    
  } else {
    
    e3 <- ggplot_add(e2,e1)  
    
  }
  
  e3
}