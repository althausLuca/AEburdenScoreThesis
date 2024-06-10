hist_plot <- function(scores, title = "Scores Distribution",
                      fill = "blue", xlim = NULL, zero_count = FALSE, mean = FALSE , title_size = 13) {

  if (hasName(scores[[1]],"score")) {
    scores <- sapply(scores, function(subject) subject$score)
  }

  if (is.null(xlim)) {
    xlim <- max(scores) + 10
  }

  p <- ggplot(data.frame(score = scores), aes(x = score))

    if((lu <- length(unique(scores)))<=10){
      p<- p+ geom_histogram(bins = lu , fill = fill, color = "black")
    }
      else{
      p<- p+ geom_histogram(bins = round(length(scores)/10+10) , fill = fill, color = "black")
    }
    p<- p + ggtitle(paste(title)) +
    # xlim(0, xlim) +
    xlab("Score") +
    ylab("Frequency") + theme(plot.title = element_text(size=title_size))


  annotation = ""
  if (zero_count) {
    annotation = paste(annotation, "\n#Zero:", sum(scores == 0))
  }
  if (mean) {
    annotation = paste(annotation, "\nMean:", round(mean(scores), 2))
  }
  if (annotation != "") {
    p <- p + annotate("text", x = Inf, y = Inf, label = annotation,
                      hjust = 1.1, vjust = 1.1)
  }

  p
  return(p)
}