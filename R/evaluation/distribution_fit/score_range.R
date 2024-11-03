#' @title get_x_range
#'
#' @param model a model defintion object
#' @return a vector of x values to be used in the plot depending on the support of the models underlying distribution
get_x_range <- function(model) {
  default <- c(-30, -0.001, 0, pracma::logseq(0.05, 600, n = 90))

  result <- default

  if (inherits(model, class(LOG_ANOVA())[1])) {
    c <- max(model$parameters$c)
    stopifnot(c > 0)
    result <- c(seq(-c, 0, length.out = max(10,40*(c>10))), default)
    return(sort(unique(result)))
  }

  if(inherits(model, c(class(ANOVA())[1],class(ZERO_INFLATED_NORMAL())[1]))){
    result <- c(seq(-30, 0, length.out = 40), default)
    return(sort(unique(result)))
  }

  return(result)
}
