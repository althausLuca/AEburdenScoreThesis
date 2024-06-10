#Required packages
install.packages("dplyr")
install.packages("tibble")
install.packages("tweedie")
install.packages("extraDistr")
install.packages("ggplot2")
install.packages("roxygen2")
install.packages("e1071")
install.packages("quantreg")
install.packages("here") # for Rmd directory setting
install.packages("statmod")
install.packages("tidyverse")
install.packages("testthat ")
install.packages("gridExtra")
install.packages("ggbreak")
install.packages("gamlss")


# Load documentation
roxygen2::roxygenise(".")


dir.create("data/trials" , recursive = TRUE)
dir.create("model_results/trials" , recursive = TRUE)
