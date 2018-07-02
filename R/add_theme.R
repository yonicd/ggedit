# taken from ggplot2:::* to avoid r cmd check note on using :::

add_theme <- function (t1, t2, t2name) 
{
  if (!is.theme(t2)) {
    stop("Don't know how to add RHS to a theme object", call. = FALSE)
  }
  for (item in names(t2)) {
    x <- t1[[item]]
    y <- t2[[item]]
    if (is.null(x) || inherits(x, "element_blank")) {
      x <- y
    }
    else if (is.null(y) || is.character(y) || is.numeric(y) || 
             is.logical(y) || inherits(y, "element_blank")) {
      x <- y
    }
    else {
      x <- merge_element(y, x)
    }
    t1[item] <- list(x)
  }
  attr(t1, "complete") <- is_theme_complete(t1) || is_theme_complete(t2)
  t1
}

is_theme_complete <- function (x) {
  
  isTRUE(attr(x, "complete"))
  
}