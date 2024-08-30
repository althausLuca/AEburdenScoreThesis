
#' Select files from a list of files
#'
#' This function selects files from a list of files based on the input and the environment variable TRIAL_SUB_SELECTION
#' the format of the environment variable is a "split/n_plits"
#'
#' For exmaple if the enviorment variable is set to 2/3, then the function will select the second third of the files
#'
#' @param file_list A list of files
#' @return A list of files
sub_select_files <- function(file_list,default="1/1"){
  trial_file_sub_selection <- Sys.getenv("TRIAL_SUB_SELECTION" , unset=default)
  trial_file_sub_selection <- as.numeric(unlist(strsplit(trial_file_sub_selection, "/")))

  split <- trial_file_sub_selection[1]
  n_splits <- trial_file_sub_selection[2]

  n_files <- length(file_list)
  split_size <- floor(n_files/n_splits)

  start_index <- ifelse(split==1, 1, (split-1)*split_size+1)
  end_index <- ifelse(split==n_splits, n_files, split*split_size)

  result <- file_list[start_index:end_index]
  return(result)
}

# test
# file_list <- list("a", "b" , "c" , "d" , "e" , "f", "g" , "h", "i", "j")
#
# print(unlist(sub_select_files(file_list)))
#
# print(unlist(sub_select_files(file_list, default="3/3")))
