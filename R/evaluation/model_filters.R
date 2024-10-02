exclude_qr <- function(model_repr, allowed_tau_values = c(0.5)) {
  if (grepl("tau", model_repr)) {
    tau <- readr::parse_number(strsplit(strsplit(model_repr, "tau_")[[1]][2], "_")[[1]][1])
    return(tau %in% allowed_tau_values)
  }
  return(TRUE)
}


# t1 <- "quantile_regression_tau_0.5.aa"
# t2 <- "quantile_regression_tau_0.25.aa"
# t3 <- "quantile_regression_tau_0.71_aa"
# t4 <- "quantile_regression_tau_0.5_bb"
#
# exclude_qr(t1)
# exclude_qr(t2)
# exclude_qr(t3)
# exclude_qr(t4)
