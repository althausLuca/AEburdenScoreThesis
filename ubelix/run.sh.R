#! /bin/bash
#SBATCH --time=05:00:00
#SBATCH --mem-per-cpu=10G

# Put your code below this line
module load R
R CMD BATCH --no-save --no-restore R/simulations/models_on_trial_file.R  output/$TRIAL_NAME.Rout

