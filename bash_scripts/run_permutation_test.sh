#!/bin/bash
echo "Running Permutation Test"
export N_FREE_THREADS=5 # number of threads to keep free for other tasks
sudo -E tmux new-session -d -s model_session " Rscript R/evaluation/permutation_test_method_comparison.R; read && echo 'Script completed'"
sudo tmux ls