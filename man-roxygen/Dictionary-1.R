#' @param ... list which defines the location of each plot within
#' @param ... other arguments passed on to graphics device
#' @param \dots options that are passed to ggedit
#' @param a gg theme
#' @param e1 theme object
#' @param e2 theme object
#' @param filename a character string giving the name of the file. 
#' @param geom character string of the name of the layer to remove
#' @param geom_list character vector of geoms in plot
#' @param id shiny module id variable
#' @param idx numeric of which index of geom to remove
#' @param input shinyapp input argument
#' @param item character type of gg layer
#' @param l ggplot2 object layer
#' @param newLayer ggplot layer or list of layers
#' @param obj ggplot as reactive shiny object
#' @param obj ggplot2 plot object or list of plot objects
#' @param obj.elems elements in layer
#' @param obj.in theme object in list form
#' @param obj.name name of theme object
#' @param oldGeom character string of the name of the layer to remove
#' @param oldGeomIdx numeric of which index of OldGeom to remove (default is 1)
#' @param output shinyapp output argument
#' @param p an object
#' @param p gg
#' @param p ggplot
#' @param p ggplot2 plot object
#' @param p.in ggplot plot object or list of objects
#' @param plot ggplot or list of ggplots to save, defaults to last plot displayed
#' @param session shiny session object
#' @param session shinyapp session argument
#' @param showDefaults toggle to control if the verbose output shows all the input arguments passed to the proto object (if verbose==FALSE then ignored)
#' @param this list
#' @param thisdepth numeric of current depth in recursive call
#' @param type character of label and inputId of element
#' @param verbose logical to control if the output includes script for layers and themes calls for parsing to create objects (default, verbose=F)
#' @param verbose logical to control if the output is a character of script or a theme object (default TRUE)
#' @param verbose toggle to control if the output is ggproto object (verbose==FALSE,default) or string of layer call (verbose==TRUE)
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#' @param x ggedit output of list of plots from the ggedit output
#' @param x list
#' @param x list from themeFetch
#' @param x numeric of row number
#' @param x vector or data.frame
#' @param y numeric of column number
