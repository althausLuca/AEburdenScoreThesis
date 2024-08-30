#! /bin/bash

trial_folder="data/trials/longer_event_durations/"

for file in $trial_folder*; do
    echo $file
    export TRIAL_FILE=$file
    export TRIAL_NAME=$(basename "$file" .${file##*.})
    sbatch longer_scen_var.sh
done


# shorter gap times
trial_folder="data/trials/shorter_gap_times/"

for file in $trial_folder*; do
    echo $file
    export TRIAL_FILE=$file
    export TRIAL_NAME=$(basename "$file" .${file##*.})
    sbatch shorter_scen_var.sh
done
