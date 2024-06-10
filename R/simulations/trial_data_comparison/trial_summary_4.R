source("R/methods/run_methods.R")

trial_folder <- "data/trials/"
method_results_folder <- "data/model_results/"

alpha <- 0.05

scenario_parameters <- list(
  "Scenario_1" <-c("k"),
  "Scenario_2" <-c("k","s"),
  "Scenario_3" <-c("k","l"),
  "Scenario_4" <-c("k")
)

scenario <- "Scenario_4"

# get all the files containg the scenario
files <- list.files(trial_folder, pattern = scenario)
files <- sort(files)
file <- files[1]
par(mfrow=c(3, 2))


for(file in files){



  k_value <- as.numeric(sub(".*_k_([0-9]+(?:\\.[0-9]+)?).*", "\\1", file))

  if(k_value > 3 ){
    next
  }

  print(file)



  ##load trials
  trial_file_name <- paste0(trial_folder, file)
  data <- read.table(trial_file_name, header = FALSE, sep = ",")
  samples_per_group <- nrow(data)/2

  df.control <- data.frame(data[data[,1] == "control",-1],stringsAsFactors = FALSE)
  df.control <- data.frame(lapply(df.control, function(x) as.numeric(as.character(x))), stringsAsFactors = FALSE)
  control_mean <- mean(colMeans(df.control))
  control_max <- max(df.control)

  df.treatment <- data.frame(data[data[,1] == "treatment",-1],stringsAsFactors = FALSE)
  df.treatment <- data.frame(lapply(df.treatment, function(x) as.numeric(as.character(x))), stringsAsFactors = FALSE)
  treatment_mean <- mean(colMeans(df.treatment))
  treatment_max <- max(df.treatment)


  print(paste("k:",k_value))
  print(paste("control mean:" , control_mean))
  print(paste("treatment mean:" , treatment_mean))
  print(paste("control max:" , control_max))
  print(paste("treatment max:" , treatment_max))

  #create histogramm plot for
  hist( rowSums(df.control==0),breaks=20 , main = paste("control", " k=",k_value))
  hist( rowSums(df.treatment==0),breaks=20 , main = paste("treatment", " k=",k_value))


  ##load method_results

  data <- read.csv(paste0(method_results_folder,file))
  p_values <- get_values(data, "p_value")
  # remove quantile_regression.rank
  p_values <- p_values[,!(names(p_values) %in% c("quantile_regression.nid", "wilcoxon" , "quantile_regression"))]
  sig_p_value_means <- colMeans(p_values < alpha)
  print(sig_p_value_means)
}


