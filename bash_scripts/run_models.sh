#!/bin/bash
echo "Running models"
sudo tmux new-session -d "sudo Rscript R/simulations/model_fitting/fit_models.R &&  echo 'Script completed'"


