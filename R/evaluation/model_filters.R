
exclude_qr <- function(model){
return(grepl("quantile_regression", model$name))
}