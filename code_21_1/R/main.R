source("R/Scenarios.R")
source("R/trials/subject.R")
#uncoment to recompute the trials
scenario <- SCENARIOS$severity

source("R/trials/trial_simulation.R")


score_df <-  data.frame(score=numeric() , group=c())
for(i in 1:10){
  result_df <-  simulate_trial_from_scenario(scenario,180)
  score_df <- rbind(score_df,result_df)
}


treatment.scores <- score_df[score_df$group=="treamtment",]
control.scores <- score_df[score_df$group=="control",]


source("R/trials/trial_analysis.R")
hist_plot(treatment.scores$score,zero_count = TRUE , mean = TRUE)
hist_plot(control.scores$score,zero_count = TRUE , mean = TRUE)