# install all required libraries
#dependening on your R version you may need:
#install.packages("remotes")
#remotes::install_version("Matrix", version = "1.6-0", repos = "https://cloud.r-project.org/")
# the glmmTMB package requires the lognormal function which is available in version 1.1.9 and not below

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
install.packages("reshape2")
install.packages("fitdistrplus")
install.packages("glmmTMB") # version 1.1.9
# remotes::install_version("glmmTMB", version = "1.1.9")
install.packages("pracma")


# Check if the version is correct
if(!packageVersion("glmmTMB") >= "1.1.9"){
  stop("Please install the correct version of glmmTMB otherwise the zero_inflated log_normalmodels will not work. \n
       version must be >= 1.1.9 (lognormal() must be defined)")
}