#' @title Convert ggplot object to a string call
#' @description Convert ggplot object to a string call
#' @param obj compiled ggplot object
#' @param file either a character string naming a file or a \link{connection}. " " indicates output to the console, Default: " "
#' @return character
#' @examples
#'  pList$pointSmooth #original compiled plot
#'  this.gg <- dput.ggedit(pList$pointSmooth) #dput the plot
#'  writeLines(this.gg) #show the output
#'  eval(parse(text=this.gg)) #recompile the plot
#'
#'  #add theme change
#'  p<-pList$pointSmooth+theme(panel.background = element_rect(fill='green'))
#'  this.gg<-dput.ggedit(p) #dput the plot
#'  writeLines(this.gg) #show the output
#'  eval(parse(text=this.gg)) #recompile the plot
#'
#' @rdname dput.ggedit
#' @export

dput.ggedit <- function(obj, file="") {
  if ("UpdatedPlots" %in% names(obj)) {
    calls <- sapply(
      grep("UpdatedScalesCalls", names(obj), value = TRUE),
      function(y) {
        sapply(obj[[y]], function(x) {
          paste0(x, collapse = "+")
        })
      }
    )

    if (is.null(dim(calls))) {
      nm <- names(calls)

      calls <- matrix(calls, nrow = 1)

      rownames(calls) <- unique(gsub("^[^.]*.", "", nm))
    }

    root.call <- sapply(obj$UpdatedPlots, cloneRoot, verbose = TRUE)
    facet.call <- sapply(obj$UpdatedPlots, function(x) {
      cF <- cloneFacet(x$facet, verbose = TRUE)

      if (is.null(cF)) {
        list()
      } else {
        cF
      }
    })

    calls <- cbind(root.call, calls, facet.call)

    str.out <- apply(calls, 1, function(obj) {
      paste(obj[!obj %in% c("list()", "")], collapse = "+\n")
    })
  } else {
    if (!inherits(obj, "gglist")) {
      if (all(sapply(obj, function(x) inherits(x, "ggplot")))) {
        objs <- obj
      } else {
        objs <- as.gglist(list(p = obj))
      }
    } else {
      objs <- obj
    }


    str.out <- sapply(objs, function(obj) {
      obj.call <- c(
        cloneRoot(obj, verbose = TRUE),
        sapply(obj$layers, cloneLayer, verbose = TRUE, showDefaults = FALSE),
        cloneFacet(obj$facet, verbose = TRUE)
      )

      if (inherits(obj$theme, "theme")) {
        obj.call <- c(obj.call, compare(obj$theme, theme_get()))
      }

      paste0(obj.call, collapse = "+\n")
    })
  }

  if (nchar(file) > 0) {
    str.out <- paste(str.out, collapse = "\n")

    cat(str.out, file = file)
  } else {
    str.out
  }
}
