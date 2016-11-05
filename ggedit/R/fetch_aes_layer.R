fetch_layer_aes=function (self, data=data.frame(x=1), params = list()){
  
  missing_aes <- setdiff(names(self$default_aes), names(data))
  if (ggplot2:::empty(data)) {
    data <- plyr::quickdf(self$default_aes[missing_aes])
  }
  else {
    data[missing_aes] <- self$default_aes[missing_aes]
  }
  aes_params <- intersect(self$aesthetics(), names(params))
  ggplot2:::check_aesthetics(params[aes_params], nrow(data))
  data[aes_params] <- params[aes_params]
  data[,-1]
}