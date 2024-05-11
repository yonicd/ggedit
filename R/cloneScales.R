#' @title Clone ggplot2 scales from compiled ggplot objects
#' @description Clone ggplot2 scales from compiled ggplot objects returns not
#' the original call but the nested call
#' @param p ggplot object
#' @param verbose boolean, if TRUE then returns script to eval(parse) if FALSE returns new compiled object Default: FALSE
#' @return ggplot scale or script (depends on verbose)
#' @examples
#'  # p <- pList$pointSmooth+scale_colour_continuous(low='red')
#'  # p
#'  # pList$pointSmooth+cloneScales(p)
#' @export
#' @rdname cloneScales
cloneScales <- function(p, verbose=FALSE) {
  sapply(p$scales$scales, function(obj) {

    obj_list <- as.list(obj$call)
    scaleout <- do.call(as.character(obj_list[[1]]), obj_list[-1])
    if (verbose) {
      scale_return <- gsub('"', "'", toString(as.expression(obj$call)))
    } else {
      scale_return <- scaleout
    }
    return(scale_return)
  })
}

make_proto_method_str <- function(self, f) {
  args <- formals(f)
  # is.null is a fast path for a common case; the %in% check is slower but also
  # catches the case where there's a `self = NULL` argument.
  has_self <- !is.null(args[["self"]]) || "self" %in% names(args)
  if (has_self) {
    fun <- quote(structure(function(...) f(..., self = self),class="ggproto_method"))
  } else {
    ff <- format(f)
    fun <- sprintf('structure(%s,class="ggproto_method")', paste0(ff, collapse = "\n"))
  }

  fun
}
