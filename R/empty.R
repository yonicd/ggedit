empty=function (df) 
{
  is.null(df) || nrow(df) == 0 || ncol(df) == 0
}