source("R/trials/subject.R")
source("R/AdverseEvents/AdverseEvents.R")
source("R/trials/trial_simulation.R")

AEs <- list(AE(3, 50) , AE(5, 50),AE(10,150))

subjects <- create_subjects(AEs, susceptibility_parameter = list("gamma", 1), group = "treatment", sample_size = 100)

subjects_simulated <- lapply(subjects, function(s) simulate_subject(s, max_time=180))

subjects_plots(subjects_simulated)
