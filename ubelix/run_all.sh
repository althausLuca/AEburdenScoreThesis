#! /bin/bash

trial_folder="data/trials/"

for file in $trial_folder*; do
    echo $file
    export TRIAL_FILE=$file
    export TRIAL_NAME=$(basename "$file" .${file##*.})
    sbatch run.sh
done



