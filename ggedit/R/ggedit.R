#' @export

ggedit <- function(p.in,viewer=paneViewer(minHeight = 1000),...) {

  if(!Sys.getenv("RSTUDIO") == "1") viewer=browserViewer()
  
  if(is.ggplot(p.in)) p.in=list(p.in)
  
  if(is.null(names(p.in))) names(p.in)=as.character(1:length(p.in))
  
  p.names=split(1:length(p.in),names(p.in))
  
  if(!all(unlist(lapply(p.in,is.ggplot)))) stop("'object' is not a valid ggplot object")
  
  if(!exists('minHeight')) minHeight=1000
  
  if(deparse(substitute(viewer))=='paneViewer()') viewer=paneViewer(minHeight)
  
  assign('.minHeight',envir = ggedit:::.ggeditEnv,minHeight)
  assign('.p',envir = ggedit:::.ggeditEnv,p.in)
  ggeditWidget()
  }