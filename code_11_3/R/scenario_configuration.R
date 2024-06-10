MILD <- "mild"
MODERATE <- "moderate"
SEVERE <- "severe"
DEATH <- "death"

SEVERITIES <- c(MILD, MODERATE, SEVERE)

SEVERETY_WEIGHTS <- c(
  "mild" = 1,
  "moderate" = 2,
  "severe" = 3,
  "death" = 4 # 4*n_events
)

TIME_UNTIL_DEATH <- c(
  "mild" = 1000,
  "moderate" = 50,
  "severe" = 10
)

SEVERITY_PROBABILITIES <- list(
  "equal" = c(1 / 3, 1 / 3, 1 / 3),
  "low" = c(0.8, 0.15, 0.05),
  "medium" = c(0.1, 0.8, 0.1),
  "high" = c(0.05, 0.15, 0.8)
)