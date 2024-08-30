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

