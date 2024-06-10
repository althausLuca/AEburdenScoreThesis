source("R/trials/trial_analysis.R")
source("R/trials/susceptibility.R")
library(gridExtra)

#' @title new Subject object
#' @description Create a new Subject object
#' @param AEs A list of Adverse Events
#' @param susceptibility A numeric value representing the susceptibility of the subject
#' @param group A string representing the group of the subject
#' @return An object of class Subject
  new_Subject <- function(AEs, susceptibility = 1, group = "treatment") {
  properties <- list(
    susceptibility = susceptibility,
    group = group,
    death_time = NULL,
    death = FALSE,
    AEs = AEs,
    score = NULL
  )
  subject <- structure(properties, class = "Subject")

  return(subject)
}

#' @title create subjects
#' @description Create a list of Subject objects
#' @param AEs A list of Adverse Events
#' @param susceptibility A list of two elements: the first element is a string representing the type of distribution,
#' options are: "constant" or "gamma" where the second element is the shape parameter of the gamma distribution
create_subjects <- function(AEs, susceptibility_parameter = list("gamma", 2), group = "treatment", sample_size = 100) {
  susceptibilities <- get_suscebilities(sample_size, susceptibility_parameter[[1]], susceptibility_parameter[[2]])
  subjects <- lapply(1:sample_size, function(k) new_Subject(AEs, susceptibilities[k], group))
  return(subjects)
}


# #' @title apply  susceptibility
# #' @description apply the susceptibility factor to the adverse events
# #' @param AEs A list of Adverse Events
# #' @param susceptibility_factor A numeric value representing the susceptibility of the subject
# #' @return A list of Adverse Events with scaled parameters
# apply_susceptibility <- function(AEs, susceptibility_factor) {
#   AEs <- lapply(AEs, function(AE) {
#     AE$severity <- AE$severity
#     AE$time_to_first_event <- AE$time_to_first_event * 1 / susceptibility_factor
#     #AE$event_duration <- AE$event_duration * susceptibility_factor
#     AE$time_between_events <- AE$time_between_events * 1 / susceptibility_factor
#     return(AE)
#   })
#   return(AEs)
# }


subjects_plots <- function(subjects, save = "") {
  # plot the score distribution
  ## plot hsit with seperate bin for 0
  p1 <- hist_plot(subjects, title = "Score distribution", zero_count = TRUE, mean = TRUE)
  # plot subject susceptibility
  susceptibilities <- sapply(subjects, function(subject) subject$susceptibility)
  p2 <- hist_plot(susceptibilities, title = "Susceptibility distribution", mean = TRUE)
  if (save != "") {
    png(paste0("plots/subjects/", save , ".png"), width = 16, height = 10, units = "cm", res=600)
    grid.arrange(p1, p2, ncol = 2)
    dev.off()
  }
  else{
    print(grid.arrange(p1, p2, ncol = 2))
  }
}

