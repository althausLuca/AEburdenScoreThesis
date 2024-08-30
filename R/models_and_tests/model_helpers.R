check_data <- function(trial) {
  if (!("Score" %in% names(trial))) {
    stop("Score column not found in trial data")
  }
  if (!("Group" %in% names(trial))) {
    stop("Group column not found in trial data")
  }
  trial$Group <- as.factor(trial$Group)
  trial$Group <- relevel(trial$Group, ref = "control")
  return(trial)
}