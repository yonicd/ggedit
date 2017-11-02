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
"+.gg" <- function(e1, e2) {

  # check for na names in themes

  if (inherits(e1, "gglist")) {
    e3 <- as.gglist(
      plyr::llply(e1, function(t1) {
        add_ggplot(t1, e2)
      })
    )
  } else {
    if (inherits(e1, "theme")) {
      idx <- which(is.na(names(e2)))

      if (length(idx) > 0) {
        e2[idx] <- NULL
      }

      e3 <- add_theme(e1, e2)
    } else {
      e3 <- add_ggplot(e1, e2)
    }
  }

  attributes(e3) <- attributes(e1)

  e3
}
