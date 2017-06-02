#' @title Interactive shiny gadget for editing ggplot layers and themes.
#' @export
#' @description Shiny gadget that takes an input ggplots and populates a user 
#' interface with objects that let the user update aesthetics of layers 
#' and theme elements.
#' @param p.in ggplot plot object or list of objects
#' @param \dots options that are passed to ggedit
#' @details
#' The user can start the gadget using the console \code{ggedit(plotobj)} or through the Addins menu in Rstudio. 
#' 
#' If you are using the the Addin option highlight on the editor window the ggplot object and then click the addin.
#' 
#' \strong{Options to pass to ggedit}
#' 
#' \strong{viewer} shiny viewer options. It can be either paneViewer (default with minHeight=1000), dialogViewer, browserViewer
#' 
#' \strong{verbose} logical to control if the output includes script for layers and themes calls for parsing to create objects (default, verbose=TRUE)
#' 
#' \strong{showDefaults} toggle to control if the verbose output shows all the input arguments passed to the proto object (if verbose==FALSE then ignored)
#' 
#' \strong{width,height} dimensions of the renderPlot where the active plot is displayed
#' 
#' Once the gadget is running the list of plots are shown in a grid and a number of objects will appear above them.
#' 
#' \strong{Action buttons}
#' 
#' Cancel: 
#' 
#' Returns a NULL object
#' 
#' Done: 
#' 
#' Returns the list decribed below. 
#' 
#' \strong{Dropdown list} 
#' 
#' Navigates through the plots in the input list. If the input list is a named list the names will be in the dropdown. The plot chosen is termed as the "active plot"
#' 
#' \strong{Radio buttons} 
#' 
#' The options to choose in the radio buttons are the layer names in the active plot.
#' 
#' \strong{Links}
#' 
#' Update Plot Layer: 
#' 
#' A pop up window will appear and be populated with aesthetic elements found in the layer chosen from the radio buttons.
#' The layer is cloned using \code{\link{cloneLayer}} creating a layer independent of the original plot.

#' If the aesthetic is a factor the values will be shown in dropdown lists. 
#' If it is numeric it will be shown in a slider. 
#' If it is a factor colour/fill aesthetic the \code{\link[colourpicker]{colourPicker}} package will allow to choose from the full pallete of colours.
#' If the continuous colour/fill aesthetic a dropdown list will be shown with different palletes
#' 
#' Update Plot Theme:
#' 
#' A popup modal will appear populated with the theme elements found in the active plot.
#' Each element will appear as having a value or empty depending if it was defined or not.
#' The user can change or fill in any element \href{http://docs.ggplot2.org/current/theme.html}{with valid values} 
#' and any textboxes left empty will use ggplot defaults.
#' 
#' Update Grid Theme:
#' 
#' Copies the theme of the active plot to the other plots in the list 
#' 
#' Update Global Theme:
#' 
#' Copies the theme of the active plot to the session theme and all plots created outside of the gadget will have this theme. 
#' 
#' View Layer Code:
#' 
#' Opens an ace editor to compare the active layer initial script call and the updated script call.
#' 
#' The ggplot objects returned (layers and themes) can be used on any ggplot object.
#' @return 
#' List of elements
#' \describe{
#' \item{updatedPlots}{list containing updated ggplot objects}
#' \item{updatedLayers}{For each plot a list of updated layers (ggproto) objects}
#' \item{UpdatedLayersElements}{For each plot a list elements and their values in each layer}
#' \item{UpdatedLayerCalls}{For each plot a list of scripts that can be run directly from the console to create a layer}
#' \item{updatedScales}{For each plot a list of updated scale objects}
#' \item{UpdatedScalesCalls}{For each plot a list of scripts that can be run directly from the console to create a scale object}
#' \item{updatedThemes}{For each plot a list of updated theme objects}
#' \item{UpdatedThemeCalls}{For each plot a list of scripts that can be run directly from the console to create a theme}
#' } 
#'
#' 
#' @seealso 
#' \code{\link{cloneLayer}},\code{\link{rgg}},\code{\link[ggplot2]{ggplot}},\code{\link[colourpicker]{colourPicker}}
#' @examples
#' p=ggplot2::ggplot(iris,ggplot2::aes(x =Sepal.Length,y=Sepal.Width))
#' p=p+ggplot2::geom_point(ggplot2::aes(colour=Species))+ggplot2::geom_line()
#' \donttest{
#' pnew=ggedit(p)
#' pnew
#' }
#' @import shiny
ggedit <- function(p.in,...) {
  requireNamespace("shinyBS")
  opts<-list(...)

  if(is.null(opts$viewer)) opts$viewer=shiny::paneViewer(minHeight = 1000)
  if(is.null(opts$verbose)) opts$verbose=TRUE
  if(is.null(opts$showDefaults)) opts$showDefaults=FALSE
  if(is.null(opts$width)) opts$width='auto'
  if(is.null(opts$height)) opts$height=600

  if(!Sys.getenv("RSTUDIO") == "1") opts$viewer=shiny::browserViewer()
  
  if(is.ggplot(p.in)) p.in=list(p.in)
  
  if(is.null(names(p.in))) names(p.in)=as.character(1:length(p.in))
  
  p.names=split(1:length(p.in),names(p.in))
  
  if(!all(unlist(lapply(p.in,is.ggplot)))) stop("'object' is not a valid ggplot object")

  assign('.p',envir = .ggeditEnv,p.in)
  assign('.verbose',envir = .ggeditEnv,opts$verbose)
  assign('.showDefaults',envir = .ggeditEnv,opts$showDefaults)
  assign('.plotWidth',envir = .ggeditEnv,opts$width)
  assign('.plotHeight',envir = .ggeditEnv,opts$height)

  ggeditGadget(viewer=opts$viewer)
  }