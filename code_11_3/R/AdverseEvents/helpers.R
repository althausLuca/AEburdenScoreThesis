source("R/scenario_configuration.R")

SEVERETY_WEIGHTS

severities_to_values <- function(severeties) {
  #if (length(severeties) == 0) {
  #  return(severeties)
  #}
  #result <- rep(0, length(severeties)) #else it gets coerced to the most general type
  #for (i in 1:length(severeties)) {
  #  result[i] <- unname(SEVERETY_WEIGHTS[severeties[i]])
  #}
  return(unname(SEVERETY_WEIGHTS[severeties]))
}







