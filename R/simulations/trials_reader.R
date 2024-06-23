
source("R/methods/run_methods.R")

data_folder <- "../../data_old/trials"
# get all the files in the foldeer that contain _k_ and _s_
files.4 <- list.files(data_folder, pattern = "Scenario_4")
files.4 <- files.4[grepl("_k_", files.4)]
files.4
files.4

file.4 <- files.4[1]
file.4
file_name <- paste0(data_folder, "/", file.4)
data <- read.table(file_name, header = FALSE, sep = "," , nrow = 10)


df.control <- data.frame(data[data[,1] == "control",-1],stringsAsFactors = FALSE)
df.control <- data.frame(lapply(df.control, function(x) as.numeric(as.character(x))), stringsAsFactors = FALSE)
control_mean <- mean(colMeans(df.control))

df.treatment <- data.frame(data[data[,1] == "treatment",-1],stringsAsFactors = FALSE)
df.treatment <- data.frame(lapply(df.treatment, function(x) as.numeric(as.character(x))), stringsAsFactors = FALSE)
treatment_mean <- mean(colMeans(df.treatment))
mild <- 1*0.6 + 2*0.3 +3*0.1
moderate <- 1*0.3 + 2*0.6 +3*0.1
severe <- 1*0.2 + 2*0.3 +3*0.5

moderate/mild
severe/mild
treatment_mean/control_mean




## check some data
file_name  <- "Scenario_2_k_10_s_0.2.csv"
file_path <-  paste0(data_folder, "/", file_name)


data <- read.table(file_path, header = FALSE, sep = "," , nrow = 10)


df.control <- data.frame(data[data[,1] == "control",-1],stringsAsFactors = FALSE)
df.control <- data.frame(lapply(df.control, function(x) as.numeric(as.character(x))), stringsAsFactors = FALSE)
control_mean <- mean(colMeans(df.control))

df.treatment <- data.frame(data[data[,1] == "treatment",-1],stringsAsFactors = FALSE)
df.treatment <- data.frame(lapply(df.treatment, function(x) as.numeric(as.character(x))), stringsAsFactors = FALSE)
treatment_mean <- mean(colMeans(df.treatment))


control_mean
treatment_mean


##lower susceptibility parameter
file_name  <- "Scenario_2_k_0.1_s_0.2.csv"
file_path <-  paste0(data_folder, "/", file_name)


data <- read.table(file_path, header = FALSE, sep = "," , nrow = 10)


df.control <- data.frame(data[data[,1] == "control",-1],stringsAsFactors = FALSE)
df.control <- data.frame(lapply(df.control, function(x) as.numeric(as.character(x))), stringsAsFactors = FALSE)
control_mean <- mean(colMeans(df.control))

df.treatment <- data.frame(data[data[,1] == "treatment",-1],stringsAsFactors = FALSE)
df.treatment <- data.frame(lapply(df.treatment, function(x) as.numeric(as.character(x))), stringsAsFactors = FALSE)
treatment_mean <- mean(colMeans(df.treatment))

control_mean
treatment_mean

