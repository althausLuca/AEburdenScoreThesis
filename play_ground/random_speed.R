make_random_generator <- function(mean_time) {
  environment <- new.env()
  environment$i <- 1
  environment$data <- rpois(100, mean_time)

  function() {
    if (environment$i > length(environment$data)) {
      environment$data <- rpois(100, mean_time)
      environment$i <- 1
    }
    value <- environment$data[environment$i]
    environment$i <- environment$i + 1
    return(value)
  }
}

# Set mean time
mean_time <- 10

# Create the generator
generator <- make_random_generator(mean_time)





# Measure time for the generator in a loop
c <- 0
generator_time <- system.time({
  for (i in 1:1000) {
    c <- c(c, generator())
  }
})

# Measure time for direct rpois calls
c <- 0
rpois_time <- system.time({
  for (i in 1:1000) {
    c <- c(c, rpois(1, mean_time))
  }
})

# Print the times
print(paste("Time for generator in loop: ", generator_time["elapsed"]))
print(paste("Time for direct rpois calls: ", rpois_time["elapsed"]))

rpois_time.direct <- system.time({
  rpois(100000, mean_time)
})


rpois_time.loop <- system.time({
  for (i in 1:100000) {
    rpois(1, mean_time)
  }
})

print(paste("Time for direct rpois calls: ", rpois_time.direct["elapsed"]))
print(paste("Time for loop rpois calls: ", rpois_time.loop["elapsed"]))

