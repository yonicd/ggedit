check_aesthetics=function (x, n) {
  
  ns <- vapply(x, length, numeric(1))
  
  good <- ns == 1L | ns == n
  
  if ( all(good) )
    return()
  
  stop("Aesthetics must be either length 1 or the same as the data (", 
       n, "): ", paste(names(!good), collapse = ", "), call. = FALSE)
  
}