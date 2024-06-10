
get_prefixed_number <- function(string , substr_prefix, as_numeric=T){
  result <- sub(paste0(".*",substr_prefix, "([0-9]+(?:\\.[0-9]+)?).*"), "\\1", string)
  if(as_numeric){
    result <- as.numeric(result)
  }
  return(result)
}