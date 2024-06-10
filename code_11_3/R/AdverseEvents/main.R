# File illustrating the use of the AdverseEvents functions

source("R/AdverseEvents/AdverseEvents.R")
source("R/AdverseEvents/event_simulation.R")
source("R/AdverseEvents/Vizualisation.R")


AE_def <- AE(10, 30,severity_probability=c(1/3,1/3,1/3) , 30)

simulated_AE <- simulate_event(AE_def, 180 , susceptibility = 1)
simulated_AE$info

plot_ae(simulated_AE, color = "green", max_time = 180, by = 10)

# low susceptibility
simulated_AE <- simulate_event(AE_def, 180 , susceptibility = 0.1)
simulated_AE$info
simulated_AE$score
plot_ae(simulated_AE, color = "green", max_time = 180)





