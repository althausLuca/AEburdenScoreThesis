source("R/data_generation/Scenarios.R")
source("R/trials/subject.R")
source("R/trials/trial_simulation.R")
source("R/trials/trial_analysis.R")
source("R/AdverseEvents/AdverseEvent.R")

severity_probabilities <- c(0.6, 0.3, 0.1)
adverse_events <- list(AE( 2, 100,severity_probabilities),
                       AE( 5, 200,severity_probabilities),
                       AE( 3, 300,severity_probabilities))

adverse_events
subjects.0 <- create_subjects(adverse_events, susceptibility = c("constant",1), group = "treatment", sample_size = 100)

# simulate subjects
subjects.0 <- lapply(subjects.0, function(subject) simulate_subject(subject, 180))
subjects_plots(subjects.0 , save="constant")


subjects.1 <- create_subjects(adverse_events, susceptibility = c("binary",0.05), group = "cont", sample_size = 1000)
subjects.1 <- lapply(subjects.1, function(subject) simulate_subject(subject, 180))
subjects_plots(subjects.1 , "binary_0.05")


for(scale_param in c(2)){
  subjects.2 <- create_subjects(adverse_events, susceptibility = c("gamma",scale_param), group = "cont", sample_size = 10000)
  subjects.2 <- lapply(subjects.2, function(subject) simulate_subject(subject, 180))
  subjects_plots(subjects.2)
}


adverse_events <- list(AE( 2, 10),
                       AE( 5, 20),
                       AE( 3, 30))

for(scale_param in c(2)){
  subjects.2 <- create_subjects(adverse_events, susceptibility = c("gamma",scale_param), group = "cont", sample_size = 10000)
  subjects.2 <- lapply(subjects.2, function(subject) simulate_subject(subject, 180))
  subjects_plots(subjects.2)
}
