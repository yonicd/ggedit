#' @title Clone ggplot facet object
#' @description Clone ggplot facet object and return either a gg object or
#'  the script to parse and evaluate
#' @param obj gg facet wrap or facet_grid object
#' @param verbose boolean, toggles to return object or script (TRUE), Default: FALSE
#' @return gg object or script
#' @examples
#'  obj=ggplot2::facet_grid(a+b~c+d,scales = 'free',as.table = FALSE,switch = 'x',shrink = FALSE)
#'
#'  cloneFacet(obj)
#'  cloneFacet(obj,verbose=TRUE)
#' @rdname clone_facet
#' @export
#' @importFrom plyr as.quoted
#' @importFrom stats as.formula
cloneFacet <- function(obj, verbose=FALSE) {
  facet_wrap_proto_class <- list(
    facets = c("formula"),
    nrow = "integer",
    ncol = "integer",
    scales = "character",
    shrink = "logical",
    labeller = c("function"),
    as.table = "logical",
    switch = "character",
    drop = "logical",
    dir = "logical",
    strip.position = "logical"
  )

  facet_grid_proto_class <- list(
    facets = c("formula"),
    margins = "logical",
    scales = "character",
    space = "character",
    shrink = "logical",
    labeller = c("function"),
    as.table = "logical",
    switch = "character",
    drop = "logical"
  )

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
      facet_wrap_p <- sapply(obj$params, function(x) if (is.list(x)) as.character(x) else x)
      facet_wrap_p$shrink <- obj$shrink
      facet_wrap_p$facets <- sprintf("~%s", paste0(facet_wrap_p$facets, collapse = "+"))
      facet_wrap_p$scales <- switch(sum(which(as.logical(c(facet_wrap_p$free)))) + 1, {
        "fixed"
      }, {
        "free_x"
      }, {
        "free_y"
      }, {
        "free"
      })
      facet_wrap_p$free <- NULL

      facet_wrap_p
    },
    FacetGrid = {
      facet_grid_p <- sapply(obj$params, function(x) if (is.list(x)) as.character(x) else x)
      facet_grid_p$shrink <- obj$shrink
      facet_grid_p$facets <- sprintf(
        "%s~%s", paste0(switch((length(facet_grid_p$rows) > 0) + 1, ".", facet_grid_p$rows), collapse = "+"),
        paste0(switch((length(facet_grid_p$cols) > 0) + 1, ".", facet_grid_p$cols), collapse = "+")
      )
      facet_grid_p$scales <- switch(sum(which(as.logical(facet_grid_p$free))) + 1, {
        "fixed"
      }, {
        "free_x"
      }, {
        "free_y"
      }, {
        "free"
      })
      facet_grid_p$space <- switch(sum(which(as.logical(facet_grid_p$space_free))) + 1, {
        "fixed"
      }, {
        "free_x"
      }, {
        "free_y"
      }, {
        "free"
      })
      facet_grid_p$free <- facet_grid_p$space_free <- NULL
      facet_grid_p$rows <- facet_grid_p$cols <- NULL

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
            formula = facet_trim[[idx]],
            logical = as.logical(facet_trim[[idx]]),
            integer = as.integer(facet_trim[[idx]])
          )
          paste(idx, rhs, sep = "=")
        }, proto_class = facet_wrap_proto_class), collapse = ",")

        sprintf("facet_wrap(%s)", facet_str)
      } else {
        facet_trim$facets <- plyr::as.quoted(stats::as.formula(facet_trim$facets))
        do.call(facet_wrap, facet_trim)
      }
    },
    FacetGrid = {
      if (verbose) {
        nm <- names(facet_proto)[names(facet_proto) %in% names(facet_trim)]
        facet_str <- paste0(sapply(nm, function(idx, proto_class) {
          rhs <- switch(proto_class[[idx]],
            character = sprintf("'%s'", facet_trim[[idx]]),
            formula = facet_trim[[idx]],
            logical = as.logical(facet_trim[[idx]]),
            integer = as.integer(facet_trim[[idx]])
          )
          paste(idx, rhs, sep = "=")
        }, proto_class = facet_grid_proto_class), collapse = ",")
        sprintf("facet_wrap(%s)", facet_str)
      } else {
        facet_trim$facets <- stats::as.formula(facet_trim$facets)
        do.call(facet_grid, facet_trim)
      }
    }
  )
}
