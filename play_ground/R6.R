Test <- R6::R6Class(classname = "test",
                    public = list(
                      width = numeric(),
                      volume = numeric(),
                      name = character(),
                      initialize = function(name = NA, width = 3, volume = 2) {
                        self$name <- name
                        self$width <- width
                        self$volume <- volume
                      }

                    )
)
test <- Test$new(name = "test", width = 3, volume = 2)
test$x <- 3