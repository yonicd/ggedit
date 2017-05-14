#' @title Backcheck what functions created the layers in a ggplot2 plot object
#' @description FUNCTION_DESCRIPTION
#' @param p gg, compiled ggplot object
#' @param obj data.frame, contains the mapping of layer functions as created in gg_session()
#' @return data.frame
#' @export 
#' @examples 
#' x<-gg_session('ggplot2')
#' p=pList$boxplotWrap
#' gg_vetting(p,x)
#' lapply(pList,gg_vetting,obj=x)
#' @importFrom plyr ldply
gg_vetting=function(p,obj){
  plyr::ldply(p$layers,proto_features)%>%inner_join(obj%>%filter(!grepl('^stat',fn)),by = c("position", "geom", "stat"))
}