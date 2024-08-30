#!/bin/bash
echo "Running models"
export N_FREE_THREADS=13 # number of threads to keep free for other tasks
sudo -E tmux new-session -d -s model_session2 " Rscript R/simulations/model_fitting/scenario_factor_variation.R ; read && echo 'Script completed'"
sudo tmux ls sudo tmux ls
