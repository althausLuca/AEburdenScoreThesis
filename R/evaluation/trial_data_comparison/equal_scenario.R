
source("R/trials/trial_loader.R")
source("R/trials/analysis/plots.R")

trial_data <- load_equal_trials()

trials <- trial_data$all_data()




max(trials$Score)

treatment.group <- trials[trials$Group == "treatment",]
names(treatment.group$Score)

control.group <- trials[trials$Group == "control",]


n_events <- ifelse(trials$Score > 0, 1, 0)

n_events.treatment <- n_events[trials$Group == "treatment"]
n_events.control <- n_events[trials$Group == "control"]

sum(n_events.control <= 2)-sum(n_events.control == 0)
sum(n_events.control == 1)
sum(n_events.control <= 1)-sum(n_events.control == 0)

sum(n_events.control[n_events.control != 1])


bin_witdh <- 1
box_lim <- 100
hist_ylim <- 800


max(n_events.treatment)
max(n_events.control)
layout(1)

plot_folder <- ""

p1 <- hist_and_box_plot(treatment.group$Score,n_events.control
  , file_name = paste0("equal", ".pdf") , main="Experimental Group"
  ,hist_lim = 100 , box_lim = box_lim
  , hist_y_lim = hist_ylim , bin_width = bin_witdh ,save=FALSE)
p2 <- hist_and_box_plot(control.group$Score, n_events.treatment , box_lim = box_lim
  , hist_y_lim = hist_ylim
  , file_name = paste0("equal_control", ".pdf")
  , hist_lim = 100 , main="Control Group", bin_width = bin_witdh,save=FALSE)
