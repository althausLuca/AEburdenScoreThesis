#! /bin/bash
#SBATCH --time=05:00:00
#SBATCH --mem-per-cpu=10G

# Put your code below this line
module load R
R CMD BATCH --no-save --no-restore R/simulations/scenario_factor_variation/longer_scenario_variation.R  output/longer_$TRIAL_NAME.Rout

