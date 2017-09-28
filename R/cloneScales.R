#' @title Clone ggplot2 scales from compiled ggplot objects
#' @description Clone ggplot2 scales from compiled ggplot objects returns not 
#' the original call but the nested call
#' @param p ggplot object
#' @param verbose boolean, if TRUE then returns script to eval(parse) if FALSE returns new compiled object Default: FALSE
#' @return ggplot scale or script (depends on verbose)
#' @examples
#'  # p <- pList$pointSmooth+scale_colour_continuous(low='red')
#'  # p
#'  # pList$pointSmooth+cloneScales(p)
#' @rdname cloneScales
cloneScales <- function(p,verbose=FALSE){
  sapply(p$scales$scales,function(obj){
    a <- as.character(obj$call)
    
    tempscale <- vector('list',length(names(formals(a[1]))))
    names(tempscale) <- names(formals(a[1]))
    
    tempscale.cl <- sapply(names(tempscale),function(idx) class(obj[[idx]]))
    
    skip.val <- grep('super',names(tempscale))
    
    for(idx in names(tempscale)[-skip.val]) tempscale[[idx]] <- obj[[idx]]
    
    tempscale$super <- formals(a[1])[['super']]
    
    scaleout <- do.call(a[1],tempscale)
    structure(scaleout, class=class(obj))
    
    if(verbose){
      scaleout_cl <- sapply(scaleout$call,class)
      idx_methods <- names(scaleout_cl)[scaleout_cl=="ggproto_method"]
      for(idx in idx_methods) scaleout$call[[idx]] <- eval(parse(text=make_proto_method_str(scaleout$call[[idx]],environment(scaleout$call[[idx]])$f)))
      browser()
      scale_return <- gsub('"',"'",paste0(format(scaleout$call),collapse='\n'))

    }else{
      scale_return <- scaleout
    }
    return(scale_return)
  })}

make_proto_method_str <- function(self, f) {
  args <- formals(f)
  # is.null is a fast path for a common case; the %in% check is slower but also
  # catches the case where there's a `self = NULL` argument.
  has_self  <- !is.null(args[["self"]]) || "self"  %in% names(args)
  if (has_self) {
    fun <- 'structure(function(...) f(..., self = self),class="ggproto_method")'
  } else {
    fun <- sprintf('structure(function(...)\n %s,class="ggproto_method")',paste0(format(f),collapse='\n'))
  }
  
  fun
}