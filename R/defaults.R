# forked from https://github.com/yihui/knitr/blob/master/R/defaults.R
#' @importFrom stats setNames
new_defaults = function(value = list()) {
  defaults = value
  
  get = function(name, default = FALSE, drop = TRUE, regex=FALSE, ...) {
    if (default) defaults = value  # this is only a local version
    if (missing(name)) defaults else {
      if (drop && length(name) == 1){
        if(regex){
          name_grep <- grep(name,names(defaults),value=TRUE, ...)
          stats::setNames(defaults[name_grep],name_grep)
        }else{
          defaults[[name]]
        }
      } else {
        stats::setNames(defaults[name], name)
      }
    }
  }
  
  set = function(...) {
    dots = list(...)
    if (length(dots) == 0) return()
    if (is.null(names(dots)) && length(dots) == 1 && is.list(dots[[1]]))
      if (length(dots <- dots[[1]]) == 0) return()
    defaults <<- merge(dots)
    invisible(NULL)
  }
  
  merge = function(values) merge_list(defaults, values)
  
  restore = function(target = value) defaults <<- target
  
  append = function(...) {
    dots = list(...)
    if (length(dots) == 0) return()
    if (is.null(names(dots)) && length(dots) == 1 && is.list(dots[[1]]))
      if (length(dots <- dots[[1]]) == 0) return()
    dots<-sapply(names(dots),function(x) dots[[x]]<-c(defaults[[x]],dots[[x]]),simplify = FALSE)
    defaults <<- merge(dots)
    invisible(NULL)
  }
  
  list(get = get, set = set, append=append, merge = merge, restore = restore)
}

#' @title Default and current ggedit options
#'
#' @description Options for functions in the ggedit package. When running R code, the object \code{ggedit_opts}
#' (default options) is not modified by chunk headers (local chunk options are
#' merged with default options), whereas \code{ggedit_opts_current} (current options)
#' changes with different chunk headers and it always reflects the options for
#' the current chunk.
#'
#' Normally we set up the global options once in the first code chunk in a
#' document using \code{ggedit_opts$set()}, so that all \emph{latter} chunks will
#' use these options. Note the global options set in one chunk will not affect
#' the options in this chunk itself, and that is why we often need to set global
#' options in a separate chunk.
#'
#' Below is a list of default chunk options, retrieved via
#' \code{ggedit_opts$get()}:
#'
#' \Sexpr[results=verbatim]{str(ggedit::ggedit_opts$get())}
# @references Usage: \url{https://yihui.name/knitr/objects/}
#'
#   A list of available options:
#   \url{https://yihui.name/knitr/options/#chunk_options}
#' @note \code{ggedit_opts_current} is read-only in the sense that it does nothing if
#'   you call \code{ggedit_opts_current$set()}; you can only query the options via
#'   \code{ggedit_opts_current$get()}.
#' @export
#' @rdname ggeditOpts
#' @examples ggedit_opts$get('themeDefaultClass')
ggedit_opts = new_defaults(list(
  
  fontDefaults=c('sans',
                 'Canonical',
                 'mono',
                 'Courier',
                 'Helvetica',
                 'serif',
                 'Times',
                 'AvantGarde',
                 'Bookman',
                 'Helvetica-Narrow',
                 'NewCenturySchoolbook',
                 'Palatino',
                 'URWGothic',
                 'URWBookman',
                 'NimbusMon',
                 'URWHelvetica',
                 'NimbusSan',
                 'NimbusSanCond',
                 'CenturySch',
                 'URWPalladio',
                 'URWTimes',
                 'NimbusRom'),
  slideDefaults=list(alpha=c(min=0, max=1),
                     size=c(min=0, max =10),
                     shape=c(min=1,max=25),
                     stroke=c(min=0, max =10),
                     weight=c(min=0, max =10),
                     linetype=c(min=1, max =5),
                     width=c(min=0, max=1),
                     angle=c(min=0,max=360),
                     hjust=c(min=-10,max=10),
                     vjust=c(min=-10,max=10),
                     stroke=c(min=0, max =10),
                     lineheight=c(min=0, max =10),
                     fontface=c(min=1, max =4),
                     rel_min_height=c(min=0,max=1),
                     scale=c(min=0,max=100)
                     ),
  themeTips=list(
    element_rect=list(
      fill='fill colour',
      colour='border colour',
      size='border size (in pts)',
      linetype=paste0(paste(seq(0,6),
                            c("blank","solid", "dashed", "dotted", "dotdash", 
                              "longdash", "twodash"),sep=": "),
                      collapse=", ")),
    element_line=list(
      colour='line colour',
      size='numeric (in pts) or \n relative to global size rel(numeric)',
      linetype=paste0(paste(seq(0,6),
                            c("blank","solid", "dashed", "dotted", "dotdash", 
                              "longdash", "twodash"),sep=": "),
                      collapse=", "),
      lineend=c("butt(default),round,square")),
    element_text=list(family=HTML('<a href="http://www.cookbook-r.com/Graphs/Fonts/" target="_blank">font family</a>'),
                      face='font face ("plain", "italic", "bold", "bold.italic")',
                      colour='text colour',
                      size='text size (in pts)',
                      hjust='horizontal justification (in [0, 1])',
                      vjust='vertical justification (in [0, 1])',
                      angle='angle (in [0, 360])',
                      lineheight='numeric line height'),
    justification=list(justification='anchor point for positioning legend inside plot <br/> "center" or two-element numeric vector'),
    position=list(position='the position of legends. <br/> "left", "right", "bottom", "top", or two-element numeric vector')
  ),
  
ThemeDefaultClass=
  data.frame(item=c('angle','background','caption','colour','face','family','fill','grid.major','grid.minor','hjust','justification','key','key.size','line','lineheight','linetype','margin','ontop','position','size','subtitle','switch.pad.grid','switch.pad.wrap','text','text.x','text.y','ticks','ticks.length','title','title.x','title.y','vjust','placement'),
                               class=c('numeric','character','character','character','character','character','character','character','character','numeric','character','character','character','character','numeric','numeric','numeric','character','character','numeric','character','character','character','character','character','character','numeric','numeric','character','character','character','numeric','character'),stringsAsFactors = FALSE),
session_geoms=
  data.frame(
  fn = c("annotation_custom", "annotation_logticks", 
         "annotation_map", "annotation_raster", "annotation_ticks", "geom_abline", 
         "geom_area", "geom_bar", "geom_bin2d", "geom_blank", "geom_boxplot", 
         "geom_col", "geom_contour", "geom_count", "geom_crossbar", "geom_curve", 
         "geom_density", "geom_density_2d", "geom_density2d", "geom_dotplot", 
         "geom_errorbar", "geom_errorbarh", "geom_freqpoly", "geom_hex", 
         "geom_histogram", "geom_hline", "geom_jitter", "geom_label", 
         "geom_line", "geom_linerange", "geom_map", "geom_path", "geom_point", 
         "geom_pointrange", "geom_polygon", "geom_qq", "geom_quantile", 
         "geom_raster", "geom_rect", "geom_ribbon", "geom_rug", "geom_segment", 
         "geom_sf", "geom_smooth", "geom_spoke", "geom_step", "geom_text", 
         "geom_tile", "geom_violin", "geom_vline", "stat_bin", "stat_bin_2d", 
         "stat_bin_hex", "stat_bin2d", "stat_binhex", "stat_boxplot", 
         "stat_contour", "stat_count", "stat_density", "stat_density_2d", 
         "stat_density2d", "stat_ecdf", "stat_ellipse", "stat_function", 
         "stat_identity", "stat_qq", "stat_quantile", "stat_sf", "stat_smooth", 
         "stat_sum", "stat_summary", "stat_summary_2d", "stat_summary_bin", 
         "stat_summary_hex", "stat_unique", "stat_ydensity"), 
  geom = c("GeomCustomAnn", 
           "GeomLogticks", "GeomAnnotationMap", "GeomRasterAnn", "GeomTicks", 
           "GeomAbline", "GeomArea", "GeomBar", "GeomTile", "GeomBlank", 
           "GeomBoxplot", "GeomCol", "GeomContour", "GeomPoint", "GeomCrossbar", 
           "GeomCurve", "GeomDensity", "GeomDensity2d", "GeomDensity2d", 
           "GeomDotplot", "GeomErrorbar", "GeomErrorbarh", "GeomPath", "GeomHex", 
           "GeomBar", "GeomHline", "GeomPoint", "GeomLabel", "GeomLine", 
           "GeomLinerange", "GeomMap", "GeomPath", "GeomPoint", "GeomPointrange", 
           "GeomPolygon", "GeomPoint", "GeomQuantile", "GeomRaster", "GeomRect", 
           "GeomRibbon", "GeomRug", "GeomSegment", "GeomSf", "GeomSmooth", 
           "GeomSpoke", "GeomStep", "GeomText", "GeomTile", "GeomViolin", 
           "GeomVline", "GeomBar", "GeomTile", "GeomHex", "GeomTile", "GeomHex", 
           "GeomBoxplot", "GeomContour", "GeomBar", "GeomArea", "GeomDensity2d", 
           "GeomDensity2d", "GeomStep", "GeomPath", "GeomPath", "GeomPoint", 
           "GeomPoint", "GeomQuantile", "GeomRect", "GeomSmooth", "GeomPoint", 
           "GeomPointrange", "GeomTile", "GeomPointrange", "GeomHex", "GeomPoint", 
           "GeomViolin"), 
  position = c("PositionIdentity", "PositionIdentity", 
               "PositionIdentity", "PositionIdentity", "PositionIdentity", "PositionIdentity", 
               "PositionStack", "PositionStack", "PositionIdentity", "PositionIdentity", 
               "PositionDodge", "PositionStack", "PositionIdentity", "PositionIdentity", 
               "PositionIdentity", "PositionIdentity", "PositionIdentity", "PositionIdentity", 
               "PositionIdentity", "PositionIdentity", "PositionIdentity", "PositionIdentity", 
               "PositionIdentity", "PositionIdentity", "PositionStack", "PositionIdentity", 
               "PositionJitter", "PositionIdentity", "PositionIdentity", "PositionIdentity", 
               "PositionIdentity", "PositionIdentity", "PositionIdentity", "PositionIdentity", 
               "PositionIdentity", "PositionIdentity", "PositionIdentity", "PositionIdentity", 
               "PositionIdentity", "PositionIdentity", "PositionIdentity", "PositionIdentity", 
               "PositionIdentity", "PositionIdentity", "PositionIdentity", "PositionIdentity", 
               "PositionIdentity", "PositionIdentity", "PositionDodge", "PositionIdentity", 
               "PositionStack", "PositionIdentity", "PositionIdentity", "PositionIdentity", 
               "PositionIdentity", "PositionDodge", "PositionIdentity", "PositionStack", 
               "PositionStack", "PositionIdentity", "PositionIdentity", "PositionIdentity", 
               "PositionIdentity", "PositionIdentity", "PositionIdentity", "PositionIdentity", 
               "PositionIdentity", "PositionIdentity", "PositionIdentity", "PositionIdentity", 
               "PositionIdentity", "PositionIdentity", "PositionIdentity", "PositionIdentity", 
               "PositionIdentity", "PositionDodge"), 
  stat = c("StatIdentity", 
           "StatIdentity", "StatIdentity", "StatIdentity", "StatIdentity", 
           "StatIdentity", "StatIdentity", "StatCount", "StatBin2d", "StatIdentity", 
           "StatBoxplot", "StatIdentity", "StatContour", "StatSum", "StatIdentity", 
           "StatIdentity", "StatDensity", "StatDensity2d", "StatDensity2d", 
           "StatBindot", "StatIdentity", "StatIdentity", "StatBin", "StatBinhex", 
           "StatBin", "StatIdentity", "StatIdentity", "StatIdentity", "StatIdentity", 
           "StatIdentity", "StatIdentity", "StatIdentity", "StatIdentity", 
           "StatIdentity", "StatIdentity", "StatQq", "StatQuantile", "StatIdentity", 
           "StatIdentity", "StatIdentity", "StatIdentity", "StatIdentity", 
           "StatSf", "StatSmooth", "StatIdentity", "StatIdentity", "StatIdentity", 
           "StatIdentity", "StatYdensity", "StatIdentity", "StatBin", "StatBin2d", 
           "StatBinhex", "StatBin2d", "StatBinhex", "StatBoxplot", "StatContour", 
           "StatCount", "StatDensity", "StatDensity2d", "StatDensity2d", 
           "StatEcdf", "StatEllipse", "StatFunction", "StatIdentity", "StatQq", 
           "StatQuantile", "StatSf", "StatSmooth", "StatSum", "StatSummary", 
           "StatSummary2d", "StatSummaryBin", "StatSummaryHex", "StatUnique", 
           "StatYdensity"), 
  pkg = rep("ggplot2",76),stringsAsFactors = FALSE)
)
)

#' @rdname ggeditOpts
#' @export
ggedit_opts_current = new_defaults()

# merge elements of y into x with the same names
merge_list = function(x, y) {
  x[names(y)] = y
  x
}
