install.packages("readxl")
library(readxl)

table <- read_excel("scenarios_patterns.xlsx", col_names = FALSE, skip = 0)

table_width <- 4

first_table_column<- table[,1]

scenarios <- list()

title_index <- 1

create_scenario_df <- function(title_index) {
  scenario_title <- table[title_index, 1][[1]]
  table_data <- table[(title_index+2):(title_index+4), 2:5]
  df <- data.frame(table_data)
  colnames(df) <- table[title_index+1, 1:table_width+1]
  rownames(df) <- t(table[(title_index+2):(title_index+4), 1])
  return(list(data=df,title=scenario_title))
}

for (title_index in c(1, 7, 13  , 19 , 25 , 31)) {
  scenarios[[length(scenarios)+1]] <- create_scenario_df(title_index)
}

