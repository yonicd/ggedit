#' @title Print to console verbose outputs of objects of class ggedit
#' @description function to tidy the ggedit output to single script calls for each plot
#' @param object ggedit object
#' @param ... used to pass show.structure
#' @details use show.structure (boolean) to control if the output shows the full data.frame
#' structure or just a placeholder [data.frame], Default: FALSE
#' @export
summary.ggedit <- function(object, ...) {
  
  if (!inherits(object, "ggedit")) {
    stop("Not ggedit object")
  }

  show.structure <- FALSE

  list2env(list(...), envir = environment())

    ret <- dput.ggedit(object$UpdatedPlots)
    
    if (!show.structure) {
      ret <- gsub(',data=structure(.*?)class = "data.frame"\\)', ",\\[data.frame\\]", ret)
    }  

  writeLines(paste0("\n", names(ret), "\n", ret))
}
