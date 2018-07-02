#' @title gglist
#' @description Checks if object is a gglist object
#' @param x object
#' @return boolean
#' @rdname is.gglist
#' @export 
is.gglist <- function(x) inherits(x, 'gglist')

#' @title recasts to gglist
#' @description adds gglist class to object
#' @param plot object
#' @return gglist object
#' @rdname as.gglist
#' @export 
as.gglist <- function(plot) {
  UseMethod('as.gglist')
}

#' @export
#' @rdname as.gglist
as.gglist.gglist <- function(plot) {
  plot
}

#' @export
#' @rdname as.gglist
as.gglist.list <- function(plot) {
  structure(plot, class = c("ggedit", "gglist"))
}

#' @export
#' @rdname as.gglist
as.gglist.ggplot <- function(plot) {
  plot <- list(plot)
  structure(plot, class = c("ggedit", "gglist","gg","ggplot"))
}

#' @export
#' @rdname as.gglist
as.gglist.ggmatrix <- function(plot) {
  plot <- list(plot)
  structure(plot, class = c("ggedit", "gglist", "ggmatrix"))
}

#' @title remove layer
#' @description remove layer from gg object
#' @param e1 plot
#' @param e2 layer
#' @return gg
#' @export
#' @import ggplot2
#' @importFrom purrr map
"-.gg" <- function(e1, e2) {
  
  pf <- proto_features(e2)
  
  if (inherits(e1, "gglist")) {
    e3 <- as.gglist(
      purrr::map(e1, .f = function(t1) {
        geom <- tolower(gsub('^Geom','',pf$geom))
        layers <- t1$layers
        layer.type <- lapply(t1$layers, function(x) class(x$geom))
        a.rm <- which(grepl(paste0("(?i)", geom), layer.type))
        if(length(a.rm)>0){
          remove_geom(t1, geom,length(a.rm)) 
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
    e3 <- remove_geom(e1, geom, length(a.rm))
  }
  
  attributes(e3) <- attributes(e1)
  
  e3
}

#' @title add layer
#' @description add layer from gg object
#' @param e1 plot
#' @param e2 layer
#' @return gg
#' @export
"+.gg" <- function(e1, e2) {

  e3 <- NULL
  
  e2name <- deparse(substitute(e2))
  
  if(is.gglist(e1)){

    e3 <- lapply(e1,ggplot2::ggplot_add, object=e2, object_name=e2name)

    attributes(e3) <- attributes(e1)

  } else {

    if      (is.theme(e1))  e3 <- add_theme(e1, e2, e2name)
    else if (is.ggplot(e1)) e3 <- add_ggplot(e1, e2, e2name)
    else if (is.ggproto(e1)) {
      stop("Cannot add ggproto objects together.",
           " Did you forget to add this object to a ggplot object?",
           call. = FALSE)
    }

  }

  e3
}

# @export
# ggplot_add.gglist <- function(object, plot, object_name) {
#   
#   e3 <- lapply(plot,ggplot2::ggplot_add,object=object)
#     
#   attributes(e3) <- attributes(plot)
#   
#   e3
# }