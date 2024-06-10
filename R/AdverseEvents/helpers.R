source("R/scenario_configuration.R")

severities_to_values <- function(severeties) {
  return(unname(SEVERETY_WEIGHTS[severeties]))
}







