r_files <- list.files(path = "../../tests/AETests", pattern = "\\.R$", full.names = TRUE)
lapply(r_files, function(f_name) {
  print(f_name)
  rm(list =  setdiff(ls(), c("f_name", "r_files")))
  source(f_name)
  return(TRUE)}
)
