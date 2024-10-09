source("R/trials/trial_loader.R")
source("R/models_and_tests/model_computer.R")
source("R/simulations/default_models.R")

MODELS <- DEFAULT_MODELS

file_path <- Sys.getenv("TRIAL_FILE" , unset="")

result_path <- gsub("data/trials/", "results/", file_path)

#replace .csv with .RData
result_path <- gsub(".csv", ".RData", result_path)

result_folder <- dirname(result_path)
model_computer_name <- gsub(".csv", ".RData", basename(file_path))

trial_data <- get_trial_data(file_path, result_path = "")


model_computer <- init_model_computer(trial_data, model_computer_name, result_folder)
add_models(model_computer, MODELS)

model_computer_name_qr <- paste0( model_computer_name , "_qr")
model_computer_name_qr <- init_model_computer(trial_data, model_computer_name_qr, result_folder)
add_models(model_computer_name_qr, QR_MODELS)