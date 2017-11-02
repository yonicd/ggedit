# taken from ggplot2:::* to avoid r cmd check note on using :::

add_theme <- function(t1, t2, t2name) {
  if (!is.theme(t2)) {
    stop(
      "Don't know how to add RHS to a theme object",
      call. = FALSE
    )
  }

  # Iterate over the elements that are to be updated
  for (item in names(t2)) {
    x <- t1[[item]]
    y <- t2[[item]]

    if (is.null(x) || inherits(x, "element_blank")) {
      # If x is NULL or element_blank, then just assign it y
      x <- y
    } else if (is.null(y) || is.character(y) || is.numeric(y) ||
      is.logical(y) || inherits(y, "element_blank")) {
      # If y is NULL, or a string or numeric vector, or is element_blank, just replace x
      x <- y
    } else {
      # If x is not NULL, then merge into y
      x <- merge_element(y, x)
    }

    # Assign it back to t1
    # This is like doing t1[[item]] <- x, except that it preserves NULLs.
    # The other form will simply drop NULL values
    t1[item] <- list(x)
  }

  # If either theme is complete, then the combined theme is complete
  attr(t1, "complete") <- attr(t1, "complete") || attr(t2, "complete")
  t1
}

#' Merge a parent element into a child element
#'
#' This is a generic and element classes must provide an implementation of this
#' method
#'
#' @param new The child element in the theme hierarchy
#' @param old The parent element in the theme hierarchy
#' @return A modified version of `new` updated with the properties of
#' `old`
#' @keywords internal
#' @export
#' @examples
#' new <- element_text(colour = "red")
#' old <- element_text(colour = "blue", size = 10)
#'
#' # Adopt size but ignore colour
#' merge_element(new, old)
#'
merge_element <- function(new, old) {
  UseMethod("merge_element")
}
#' @rdname merge_element
#' @export
merge_element.default <- function(new, old) {
  stop("No method for merging ", class(new)[1], " into ", class(old)[1], call. = FALSE)
}
#' @rdname merge_element
#' @export
merge_element.element <- function(new, old) {
  if (!inherits(new, class(old)[1])) {
    stop("Only elements of the same class can be merged", call. = FALSE)
  }
  # Override NULL properties of new with the values in old
  # Get logical vector of NULL properties in new
  idx <- vapply(new, is.null, logical(1))
  # Get the names of TRUE items
  idx <- names(idx[idx])

  # Update non-NULL items
  new[idx] <- old[idx]

  new
}
