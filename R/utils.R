#' @inherit magrittr::`%>%`
#' @export
`%>%` <- magrittr::"%>%"

#' @importFrom utils capture.output
#' @importFrom stats as.formula
#' @importFrom rlang quo_name
build_map <- function(item,y) {
  
  y <- y[[item]]
  
  if (inherits(y,'quosure')){
    return(sprintf('%s = %s',item,rlang::quo_name(y)))
  }
  
  if (inherits(y,'character')){
    return(sprintf("%s = '%s'",item,y))
  }
  
  if (inherits(y, "formula")){
    return(sprintf("formula=stats::as.formula('%s')",
                   paste0(as.character(y)[-1], collapse = "~")))
  }
    
  
  if (inherits(y,'NULL')) {
    return(sprintf('%s = NULL',item))
  }
  
  
  if (inherits(y, c("function", "call", "ggproto"))) {
    return(sprintf("%s = %s",
                   item,
                   paste(utils::capture.output(
                     dput(y)),
                     collapse = "\n")
    ))
  }
  
  if (inherits(y, c("data.frame"))) {
    return(paste0("=", paste(utils::capture.output(dput(y)), collapse = "\n")))
    }
  
  #if (inherits(y, c("ggproto_method"))) out <- paste0("=ggraph::get_edges(edges='short')")
  
  return(sprintf('%s = %s',item, y))

}

