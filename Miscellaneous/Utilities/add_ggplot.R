# taken from ggplot2:::* to avoid r cmd check note on using :::

add_ggplot <- function(p, object, objectname) {
  if (is.null(object)) return(p)

  p <- plot_clone(p)
  if (is.data.frame(object)) {
    p$data <- object
  } else if (is.theme(object)) {
    p$theme <- update_theme(p$theme, object)
  } else if (inherits(object, "Scale")) {
    p$scales$add(object)
  } else if (inherits(object, "labels")) {
    p <- update_labels(p, object)
  } else if (inherits(object, "guides")) {
    p <- update_guides(p, object)
  } else if (inherits(object, "uneval")) {
    p$mapping <- defaults(object, p$mapping)
    # defaults() doesn't copy class, so copy it.
    class(p$mapping) <- class(object)

    labels <- lapply(object, deparse)
    names(labels) <- names(object)
    p <- update_labels(p, labels)
  } else if (is.Coord(object)) {
    p$coordinates <- object
    p
  } else if (is.facet(object)) {
    p$facet <- object
    p
  } else if (is.list(object)) {
    for (o in object) {
      p <- p %+% o
    }
  } else if (is.layer(object)) {
    p$layers <- append(p$layers, object)

    # Add any new labels
    mapping <- make_labels(object$mapping)
    default <- make_labels(object$stat$default_aes)
    new_labels <- defaults(mapping, default)
    p$labels <- defaults(p$labels, new_labels)
  } else {
    stop(
      "Don't know how to add ", objectname, " to a plot",
      call. = FALSE
    )
  }
  set_last_plot(p)
  p
}

plot_clone <- function(plot) {
  p <- plot
  p$scales <- plot$scales$clone()

  p
}

update_theme <- function(oldtheme, newtheme) {
  # If the newtheme is a complete one, don't bother searching
  # the default theme -- just replace everything with newtheme
  if (attr(newtheme, "complete")) {
    return(newtheme)
  }

  # These are elements in newtheme that aren't already set in oldtheme.
  # They will be pulled from the default theme.
  newitems <- !names(newtheme) %in% names(oldtheme)
  newitem_names <- names(newtheme)[newitems]
  oldtheme[newitem_names] <- theme_get()[newitem_names]

  # Update the theme elements with the things from newtheme
  # Turn the 'theme' list into a proper theme object first, and preserve
  # the 'complete' attribute. It's possible that oldtheme is an empty
  # list, and in that case, set complete to FALSE.
  old.validate <- isTRUE(attr(oldtheme, "validate"))
  new.validate <- isTRUE(attr(newtheme, "validate"))
  oldtheme <- do.call(theme, c(
    oldtheme,
    complete = isTRUE(attr(oldtheme, "complete")),
    validate = old.validate & new.validate
  ))

  oldtheme + newtheme
}

update_labels <- function(p, labels) {
  p <- plot_clone(p)
  p$labels <- defaults(labels, p$labels)
  p
}

update_guides <- function(p, guides) {
  p <- plot_clone(p)
  p$guides <- defaults(guides, p$guides)
  p
}

is.layer <- function(x) inherits(x, "Layer")

make_labels <- function(mapping) {
  remove_dots <- function(x) {
    match_calculated_aes <- "^\\.\\.([a-zA-Z._]+)\\.\\.$"

    gsub(match_calculated_aes, "\\1", x)
  }

  default_label <- function(aesthetic, mapping) {
    # e.g., geom_smooth(aes(colour = "loess"))
    if (is.character(mapping)) {
      aesthetic
    } else {
      remove_dots(deparse(mapping))
    }
  }
  Map(default_label, names(mapping), mapping)
}

defaults <- function(x, y) {
  c(x, y[setdiff(names(y), names(x))])
}
