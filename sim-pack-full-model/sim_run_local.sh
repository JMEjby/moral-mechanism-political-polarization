#!/bin/sh
export RUN_MODE=local

# generate parameters
Rscript generate_params.R

# compile the simulation driver
javac -cp app/netlogo-6.4.0.jar ModelSim.java

PARAM_FILE="gen_sim_params.csv"

if [ ! -f "$PARAM_FILE" ]; then
    echo "Could not find parameter file."
    exit 1
fi

# run the model, one parameter set per loop, until the param file is exhausted
while [ -f "$PARAM_FILE" ]; do
    Rscript sim_track.R          # advances to the next parameter set
    Rscript generate_agent_mfs.R # draws agent moral foundations for this sim
    java -cp app/netlogo-6.4.0.jar:. -Dnetlogo.extensions.dir=. ModelSim
done

# combine the per-sample turtle CSVs into one raw file
csvstack ./outputs/raw_csv/*.csv > ./outputs/combined_data/Turtle_data_raw.csv
