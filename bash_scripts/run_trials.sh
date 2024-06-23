#!/bin/bash
echo "Running trials"
echo "running scenario 1"
sudo  tmux new-session -d -s s1 "Rscript R/simulations/trial_simulation/identical_settings.R; read"
echo "running scenario 2"
sudo  tmux new-session -d -s s2 "Rscript R/simulations/trial_simulation/shorter_gap_times.R; read"
echo "running scenario 3"
sudo  tmux new-session -d -s s3  "Rscript R/simulations/trial_simulation/longer_event_durations.R; read"
sudo tmux ls
#to kill all tmux sessions run the following command
#sudo tmux kill -a -t s1 s2 s3