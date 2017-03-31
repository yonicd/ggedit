ggeditAddin <- function() {
  require(ggedit)
  # Get the document context.
  context <- rstudioapi::getActiveDocumentContext()
  
  # Set the default data to use based on the selection.
  text <- context$selection[[1]]$text
  
  if (nchar(text) == 0) {
    stop('Please highlight a ggplot2 plot before selecting this addin.')
  }
  
  p.in=eval(parse(text=text))
  
  addin.out=ggedit(p.in = p.in)
  assign('p.out',envir = .GlobalEnv,value = addin.out)
}