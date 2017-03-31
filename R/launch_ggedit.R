#' @import shiny
launch_ggedit <- function(p,output,rstudio, ...)
{
  assign(x = '.p',envir = .ggeditEnv,value = p)
  assign(x = '.output',envir = .ggeditEnv,value = output)
  
  on.exit({
    #rm(list = ls(envir = .GlobalEnv)[ls(envir = .GlobalEnv)%in%c('p.new','p.in','p.theme','pList.new','p.Elems','themeUpdate')],envir = .GlobalEnv)
    assign(x = '.p',envir = .ggeditEnv,value = NULL)
    #assign(x = '.output',envir = .ggeditEnv,value = NULL)
    },add = T
    )
  shiny::runApp(appDir = system.file("application", package = 'ggedit'),...)
}