#' @title Clone ggplot facet object
#' @description Clone ggplot facet object and return either a gg object or
#'  the script to parse and evaluate
#' @param obj gg facet wrap or facet_grid object
#' @param verbose boolean, toggles to return object or script (TRUE), Default: FALSE
#' @return gg object or script
#' @examples
#'  obj <- ggplot2::facet_grid(
#'             rows = vars("a", "b"), cols = vars("c", "d"),
#'             scales = 'free', as.table = FALSE, switch = 'x', shrink = FALSE
#'         )
#'
#'  cloneFacet(obj)
#'  cloneFacet(obj, verbose = TRUE)
#' @rdname clone_facet
#' @export
#' @importFrom rlang names2
cloneFacet <- function(obj, verbose = FALSE) {
  facet_wrap_proto_class <- list(
    facets = "closure",
    nrow = "integer",
    ncol = "integer",
    scales = "character",
    shrink = "logical",
    labeller = c("function"),
    as.table = "logical",
    switch = "character",
    drop = "logical",
    dir = "logical",
    strip.position = "logical",
    axes = "character",
    axis.labels = "character"
  )

  facet_grid_proto_class <- list(
    rows = "closure",
    cols = "closure",
    scales = "character",
    space = "character",
    shrink = "logical",
    labeller = c("function"),
    as.table = "logical",
    switch = "character",
    drop = "logical",
    margins = "logical",
    axes = "character",
    axis.labels = "character",
    facets =  c("function")
  )

  map_param_opts <- function(proto_list, opts) {
    opts[sum(which(as.logical(proto_list))) + 1]
  }

  facet_proto <- switch(class(obj)[1],
    FacetWrap = {
      facet_wrap_proto <- formals(facet_wrap)
      facet_wrap_proto$labeller <- label_value
      facet_wrap_proto
    },
    FacetGrid = {
      facet_grid_proto <- formals(facet_grid)
      facet_grid_proto$labeller <- label_value
      facet_grid_proto
    }
  )

  facet_p <- switch(class(obj)[1],
    FacetWrap = {
      facet_wrap_p <- as.list(facet_proto)
      facet_wrap_p$facets <- obj$params$facets
      facet_wrap_p$nrow <- obj$params$nrow
      facet_wrap_p$ncol <- obj$params$ncol
      facet_wrap_p$scales <- map_param_opts(facet_wrap_p$params$free, c("fixed", "free_x", "free_y", "free"))
      facet_wrap_p$shrink <- obj$shrink
      facet_wrap_p$labeller <- obj$params$labeller
      facet_wrap_p$as.table <- obj$params$as.table
      facet_wrap_p$drop <- obj$params$drop
      facet_wrap_p$dir <- obj$params$dir
      facet_wrap_p$strip.position <- obj$params$strip.position
      facet_wrap_p$axes <- map_param_opts(obj$params$draw_axes, c("margins", "all_x", "all_y", "all"))
      facet_wrap_p$axis.labels <- map_param_opts(obj$params$axis_labels, c("margins", "all_x", "all_y", "all"))
      facet_wrap_p
    },
    FacetGrid = {
      facet_grid_p <- as.list(facet_proto)
      facet_grid_p$rows <- obj$params$rows
      facet_grid_p$cols <- obj$params$cols
      facet_grid_p$shrink <- obj$shrink
      facet_grid_p$labeller <- obj$params$labeller
      facet_grid_p$as.table <- obj$params$as.table
      facet_grid_p$switch <- obj$params$switch
      facet_grid_p$drop <- obj$params$drop
      facet_grid_p$margins <- obj$params$margins
      facet_grid_p$scales <- map_param_opts(obj$params$free, c("fixed", "free_x", "free_y", "free"))
      facet_grid_p$space <- map_param_opts(obj$params$space_free, c("fixed", "free_x", "free_y", "free"))
      facet_grid_p$axes <- map_param_opts(obj$params$draw_axes, c("margins", "all_x", "all_y", "all"))
      facet_grid_p$axis.labels <- map_param_opts(obj$params$axis_labels, c("margins", "all_x", "all_y", "all"))
      facet_grid_p
    }
  )


  nm_p <- names(facet_p)
  nm_p <- nm_p[!grepl('plot_env',nm_p)]

  facet_trim <- facet_p[which(!sapply(nm_p, function(x) {
    is.logical(all.equal(facet_proto[x], facet_p[x]))
  }))]

  switch(class(obj)[1],
    FacetWrap = {
      if (verbose) {
        nm <- names(facet_proto)[names(facet_proto) %in% names(facet_trim)]
        facet_str <- paste0(sapply(nm, function(idx, proto_class) {
          rhs <- switch(proto_class[[idx]],
            character = sprintf("'%s'", facet_trim[[idx]]),
            closure = sprintf("vars(%s)", gsub('"',"'",paste0(rlang::names2(facet_trim[[idx]]), collapse = ', '))),
            formula = facet_trim[[idx]],
            logical = as.logical(facet_trim[[idx]]),
            integer = as.integer(facet_trim[[idx]])
          )
          paste(idx, rhs, sep = "=")
        }, proto_class = facet_wrap_proto_class), collapse = ", ")

        sprintf("facet_wrap(%s)", facet_str)
      } else {
        do.call(facet_wrap, facet_trim)
      }
    },
    FacetGrid = {
      if (verbose) {
        nm <- names(facet_proto)[names(facet_proto) %in% names(facet_trim)]
        facet_str <- paste0(sapply(nm, function(idx, proto_class) {
          rhs <- switch(proto_class[[idx]],
            character = sprintf("'%s'", facet_trim[[idx]]),
            closure = sprintf("vars(%s)", gsub('"',"'",paste0(rlang::names2(facet_trim[[idx]]), collapse = ', '))),
            formula = facet_trim[[idx]],
            logical = as.logical(facet_trim[[idx]]),
            integer = as.integer(facet_trim[[idx]])
          )
          paste(idx, rhs, sep = "=")
        }, proto_class = facet_grid_proto_class), collapse = ", ")
        sprintf("facet_grid(%s)", facet_str)
      } else {
        do.call(facet_grid, facet_trim)
      }
    }
  )
}
