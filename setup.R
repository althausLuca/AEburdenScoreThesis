# install all required libraries


#dependening on your R version you may need:
#install.packages("remotes")
#remotes::install_version("Matrix", version = "1.6-0", repos = "https://cloud.r-project.org/")

#Required packages
install.packages("quantreg")
install.packages("dplyr")
install.packages("tibble")
install.packages("tweedie")
install.packages("extraDistr")
install.packages("ggplot2")
install.packages("roxygen2")
install.packages("e1071")
install.packages("here") # for Rmd directory setting
install.packages("statmod")
install.packages("tidyverse")
install.packages("testthat")
install.packages("gridExtra")
install.packages("ggbreak")
install.packages("gamlss")
install.packages("latex2exp")

# Load documentation
#roxygen2::roxygenise(".") #TODO test why this runs code


dir.create("data_old/trials", recursive = TRUE)
dir.create("model_results/trials" , recursive = TRUE)
