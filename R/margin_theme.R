#' @importFrom grid unitType
margin_theme <- function(a,obj,obj.class,obj.names,i) {
  if(grepl('\\.margin$',obj.names[i])){
    b <- split(
      c(
        a[[obj.names[[i]]]],
        attr(a[[obj.names[[i]]]], "unit")
      ),
      c("t", "r", "b", "l", "unit")
    )
    b$unit <- sprintf("'%s'",b$unit)
    obj$margin["value"] <- paste0(paste(names(b), b, sep = "="), collapse = ",")
  }else{
    
    midx <- grep('margin',names(obj.class))
    
    if (!is.null(a[[obj.names[[i]]]][[midx]])) {
      b <- split(
        c(
          a[[obj.names[[i]]]][[midx]],
          grid::unitType(a[[obj.names[[i]]]][[midx]])[1]
        ),
        c("t", "r", "b", "l", "unit")
      )
      b$unit <- sprintf("'%s'",b$unit)
      obj$margin["value"] <- paste0(paste(names(b), b, sep = "="), collapse = ",")
    }else{
      obj$margin["value"] <- 'NULL'
    }
  }
  
  obj
}