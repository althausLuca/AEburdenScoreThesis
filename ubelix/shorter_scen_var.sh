#! /bin/bash
#SBATCH --time=08:00:00
#SBATCH --mem-per-cpu=10G

# Put your code below this line
module load R
R CMD BATCH --no-save --no-restore R/simulations/scenario_factor_variation/shorter_scenario_variation.R output/shorter_$TRIAL_NAME.Rout

