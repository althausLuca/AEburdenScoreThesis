# Duration descriptions (days)
DURATION_SHORT <- 3
DURATION_LONG <- 7

# gap time and time to first event description
# GAP_TIME_SHORT <- 750
# GAP_TIME_LONG <- 1500

# duration multiplier
SHORTER <- 0.5
LONGER <- 3.5

# Severity descriptions
MOSTLY_MILD <- c(0.6, 0.3, 0.1)
MOSTLY_MODERATE <- c(0.3, 0.6, 0.1)
MOSTLY_SEVERE <- c(0.2, 0.3, 0.5)

MORE_MODERATE <- c(0, 0, 1)
MORE_SEVERE <- c(0, 0, 1)

map_severity <- list("mild" = MOSTLY_MILD,
                     "mostly mild" = MOSTLY_MILD,
                     "moderate" = MOSTLY_MODERATE,
                     "mostly moderate" = MOSTLY_MODERATE,
                     "severe" = MOSTLY_SEVERE,
                     "mostly severe" = MOSTLY_SEVERE,
                     "more moderate" = MORE_MODERATE,
                     "more severe" = MORE_SEVERE
)


## scenatio pattern

DEFAULT_DURATION <- c("short", "long", "short")
DEFAULT_GAP_TIMES <- c("long", "long", "short")
DEFAULT_SEVERITY <- c("mostly mild", "mostly moderate", "mostly mild")

SCENARIO_PATTERNS <- list(
  Control = list(
    description = "No treatment, all events are equally likely",
    AEs = data.frame(duration = DEFAULT_DURATION,
                     gap_time = DEFAULT_GAP_TIMES,
                     severity = DEFAULT_SEVERITY
    )
  ),
  s1 = list(
    description = "Same as the control scenario",
    name = "Scenario_1",
    AEs = data.frame(duration = DEFAULT_DURATION,
                     gap_time = DEFAULT_GAP_TIMES,
                     severity = DEFAULT_SEVERITY
    )
  ),
  s2 = list(
    name = "Scenario_2",
    description = "More frequent than control",
    AEs = data.frame(duration = DEFAULT_DURATION,
                     gap_time = c("shorter", "shorter", "shorter"),
                     severity = DEFAULT_SEVERITY
    )
  ),
  s3 = list(
    name = "Scenario_3",
    description = "Longer Duration than control",
    AEs = data.frame(duration = c("longer", "longer", "longer"),
                     gap_time = DEFAULT_GAP_TIMES,
                     severity = DEFAULT_SEVERITY
    )
  ),
  s4 = list(
    name = "Scenario_4",
    description = "More severe than control",
    AEs = data.frame(duration = DEFAULT_DURATION,
                     gap_time = DEFAULT_GAP_TIMES,
                     severity = c("more moderate", "more severe", "more moderate")
    )
  )

)

library(tibble)

load_scenario <- function(scenario, shorter = SHORTER, longer = LONGER) {
  source("R/AdverseEvents/AdverseEvent.R")
  stopifnot(scenario %in% names(SCENARIO_PATTERNS))

  control_data <- SCENARIO_PATTERNS$Control$AEs
  control <- tibble(
    duration = ifelse(control_data$duration == "short", DURATION_SHORT, DURATION_LONG),
    gap_time = ifelse(control_data$gap_time == "short", GAP_TIME_SHORT, GAP_TIME_LONG),
    severity = map_severity[control_data$severity]
  )

  treatment_data <- SCENARIO_PATTERNS[[scenario]]$AEs
  treatment <- control # copy defaults
  ## handle cases shorter and longer than control
  treatment$duration <- ifelse(treatment_data$duration == "shorter", treatment$duration * shorter,
                               ifelse(treatment_data$duration == "longer", treatment$duration * longer, treatment$duration))

  treatment$gap_time <- ifelse(treatment_data$gap_time == "shorter", treatment$gap_time * shorter,
                               ifelse(treatment_data$gap_time == "longer", treatment$gap_time * longer, treatment$gap_time))

  treatment$severity <- map_severity[treatment_data$severity]


  control_AEs <- list()
  for (i in 1:nrow(control)) {
    control_AEs[[i]] <- AE(control$duration[i], control$gap_time[i], control$severity[[i]])
  }

  treatment_AEs <- list()
  for (i in 1:nrow(treatment)) {
    treatment_AEs[[i]] <- AE(treatment$duration[i], treatment$gap_time[i], treatment$severity[[i]])
  }
  return(list(control = control_AEs, treatment = treatment_AEs, name = SCENARIO_PATTERNS[[scenario]]$name))
}