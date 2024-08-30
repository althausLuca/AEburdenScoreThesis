source("R/models/model_results.R")
source("R/model_settings.R")
library(ggplot2)


for(size in c(20,30,50)){
    file <- paste0("data/workspaces/equal_samples_", size, ".RData")
    load(file, envir =  (data_env <- environment()))
    result <- init_model_results(data_env$result)

    p_values <- result$get_values("p_value")

    p_value_plot <- p_value_plot_handler()
    for(name in names(p_values)){
        p_value_plot$add(p_values[[name]], name)
    }
    print(p_value_plot$plot())
    p_value_plot$save(paste0("p_values_equal_sample_size_",size,".pdf"))
}

for(size in c(100)){ #20,30,50,
    file <- paste0("data/workspaces/longer_samples_", size, ".RData")
    load(file, envir =  (data_env <- environment()))
    result <- init_model_results(data_env$result)

    p_values <- result$get_values("p_value")

    p_value_plot <- p_value_plot_handler()
    for(name in names(p_values)){
        p_value_plot$add(p_values[[name]], name)
    }
    print(p_value_plot$plot())
    p_value_plot$save(paste0("p_values_longer_sample_size_",size,".pdf"))
}
