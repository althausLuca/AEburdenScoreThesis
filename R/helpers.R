get_prefixed_number <- function(string, substr_prefix, as_numeric = T) {
  result <- sub(paste0(".*", substr_prefix, "([0-9]+(?:\\.[0-9]+)?).*"), "\\1", string)
  if (as_numeric) {
    result <- as.numeric(result)
  }
  return(result)
}

#' flatten list
#' @param lst list to flatten (nested lists)
#' @param sep separator for the names
#' @return flattened list
flatten_list <- function(lst, sep = ".") {
  result <- list()
  for (name in names(lst)) {
    if (!is.list(lst[[name]])) {
      result[[name]] <- lst[[name]]
    } else {
      inner_lst <- flatten_list(lst[[name]], sep)
      for (inner_name in names(inner_lst)) {
        result[[paste0(name, sep, inner_name)]] <- inner_lst[[inner_name]]
      }
    }
  }
  return(result)
}


nested_list_to_data_frame <- function(lst, sep = ".") {
  if (!is.list(lst)) {
    stop("Input is not a list")
  }
  if (length(lst) == 0) {
    return(data.frame())
  }

  lst_flattened <- lapply(lst, flatten_list)
  df <- data.frame(do.call(rbind , lst_flattened ))
  return(df)
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
