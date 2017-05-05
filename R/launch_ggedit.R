#' @import shiny
launch_ggedit <- function(p,output,rstudio, ...)
{
  assign(x = '.p',envir = .ggeditEnv,value = p)
  assign(x = '.output',envir = .ggeditEnv,value = output)
  
  on.exit({
    assign(x = '.p',envir = .ggeditEnv,value = NULL)
    },add = T
    )
  shiny::runApp(appDir = system.file("application", package = 'ggedit'),...)
}