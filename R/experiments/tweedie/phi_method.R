source("R/trials/trial_loader.R")
source("R/models_and_tests/models_and_tests.R")
source("R/evaluation/config.R", local = (eval_config <- new.env()))


model_file <- eval_config$DEFAULT_GAP_TIME_VAR_FILE
model_computer <- load_model_computer(model_file)


pearson_estimates <- model_computer$model_estimates$tweedie_var_power_infer_link_power_0
mle_estimates <-model_computer$model_estimates$tweedie_var_power_infer_link_power_0_mle

colnames(mle_estimates)

mean(unlist(phi_p <- pearson_estimates[,3]))
mean(unlist(phi_mle <- mle_estimates[,3]))

mean(unlist(xi_p <- pearson_estimates[,4]))
mean(unlist(xi_mle <- mle_estimates[,4]))

mle_1.8 <- model_computer$model_estimates$tweedie_var_power_1.8_link_power_0_mle
mle_1.2 <-model_computer$model_estimates$tweedie_var_power_1.2_link_power_0_mle
mean(unlist(phi_p <- mle_1.8[,3]))
mean(unlist(phi_mle <- mle_1.2[,3]))
mean(unlist(phi_mle <- mle_1.2[,4]))

# model <- TWEEDIE_REGRESSION(xi = "infer", link_power = 0)
# model <- force_computation(model)
# add_model(model_computer,model,save = TRUE, recompute = FALSE)
