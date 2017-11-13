#' @title Backcheck what functions created the layers in a ggplot2 plot object
#' @description Validate geoms with their unique attributes
#' @param p gg, compiled ggplot object
#' @param obj data.frame, contains the mapping of layer functions as created in gg_session(),
#' Default: ggedit_opts$get('session_geoms')
#' @return data.frame
#' @export
#' @examples
#' gg_vetting(pList$boxplotWrap)
#' lapply(pList,gg_vetting)
#' @importFrom plyr ldply
gg_vetting <- function(p, obj=ggedit_opts$get('session_geoms')) {
  plyr::ldply(p$layers, proto_features) %>%
    inner_join(
      obj %>%
        filter_(~!grepl("^stat", fn)),
      by = c("position", "geom", "stat")
    )
}
