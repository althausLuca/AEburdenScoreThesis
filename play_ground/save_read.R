
path <- "play_ground/"
output_file_name <- paste0(path,"test.csv")
output_file_conn <- file(output_file_name, "a")

results <- list(
  "model_1" = list("model" = "model_1",
                   "p_value" = 0.01),
    "model_2" = list("model" = "model_2",
                     "p_value" = 1.02),
    "model_3" = list("model" = "model_3",
                     "p_value" = NA)
)

write.table(results, output_file_conn, sep = ",", row.names = FALSE, col.names = TRUE)

# Close the file connection
close(output_file_conn)