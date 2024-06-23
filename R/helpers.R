get_prefixed_number <- function(string, substr_prefix, as_numeric = T) {
  result <- sub(paste0(".*", substr_prefix, "([0-9]+(?:\\.[0-9]+)?).*"), "\\1", string)
  if (as_numeric) {
    result <- as.numeric(result)
  }
  return(result)
}

dict <- function(..., default = NULL) {
  input_vec <- list(...)
  #call  <<- match.call()

  #expr <- '\\b[a-zA-Z_][a-zA-Z0-9_]*\\b(?=\\s*=\\s*(?!\\"))'
  #matches <- regmatches(call, gregexpr(expr, call, perl = TRUE))[[1]] # get the names of the arguments not in quotes
  g <- Vectorize(function(x) ifelse(is.character(y <- get0(x, ifnotfound = x)), y, x), USE.NAMES = FALSE)

  names(input_vec) <- g(names(input_vec))

  class(input_vec) <- "dict"
  attr(input_vec, "default") <- default

  return(input_vec)
}

# override [] for dict
`[.dict` <- function(x, i, ...) {
  if (is.character(i)) {
    result <- x[[i]]
  }
  if (is.numeric(i)) {
    result <- x[names(x) %in% i]
  }
  if (is.null(result)) {
    result <- attr(x, "default")
  }
    return(result)
}




c <- dict("a" = "a", a = 2, "b" = 3, default = "EEE")
c
c["a"]

