source("R/data_generation/AE_simulation/AdverseEvent.R")

# Severity weights
MOSTLY_MILD <- c(0.6, 0.3, 0.1)
MOSTLY_MODERATE <- c(0.3, 0.6, 0.1)
MOSTLY_SEVERE <- c(0.2, 0.3, 0.5)

if(!exists("GAP_TIME_SHORT")){
  GAP_TIME_SHORT <- 750
  GAP_TIME_LONG <- 1500
}

DURATION_SHORT <- 3
DURATION_LONG <- 7

# Default adverse events
DEFAULT_AE_TYPES <- list(
  AE(DURATION_SHORT, GAP_TIME_LONG, MOSTLY_MILD),
  AE(DURATION_LONG, GAP_TIME_LONG, MOSTLY_MODERATE),
  AE(DURATION_SHORT, GAP_TIME_SHORT, MOSTLY_MILD)
)






