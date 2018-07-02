# taken from ggplot2:::* to avoid r cmd check note on using :::

add_ggplot <- function (p, object, objectname) 
{
  if (is.null(object)) 
    return(p)
  p <- plot_clone(p)
  p <- ggplot_add(object, p, objectname)
  set_last_plot(p)
  p
}

plot_clone <- function(plot) {
  p <- plot
  p$scales <- plot$scales$clone()

  p
}
