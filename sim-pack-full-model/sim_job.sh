#!/bin/sh
# Grid Engine options (lines prefixed with #$)
#$ -N moral_sig_sims
#$ -wd /exports/eddie/scratch/s1917169/moral-mechanism-political-polarization/sim-pack-full-model
#$ -m beas
#$ -M s1917169@ed.ac.uk
#$ -l h_rt=100:00:00
#$ -l h_vmem=50G

# Initialise the environment modules
. /etc/profile.d/modules.sh

# Load R
module load R/4.4

# Load java
module load java/jdk-22.0.1

# load python for pip install
module load anaconda

if ! command -v csvstack &> /dev/null; then
    echo "csvkit not found. Installing..."
    pip install csvkit
else
    echo "csvkit is already installed."
fi

export RUN_MODE=remote

# generate parameters
Rscript generate_params.R # requires libraries

cp gen_sim_params_fixed.csv /home/s1917169/moral-mechanism-political-polarization/sim-pack-full-model # save params in a permanent location

# compile simulation file
javac -cp app/netlogo-6.4.0.jar ModelSim.java # requires csv and rnd extension folders and param and mf csv files

PARAM_FILE="gen_sim_params.csv"

if [ ! -f "$PARAM_FILE" ]; then
    echo "Could not find parameter file."
    exit 1
fi

# Run the program
while [ -f "$PARAM_FILE" ]; do
    Rscript sim_track.R # updates sim files
    Rscript generate_agent_mfs.R # requires a pre req RData to be in the same folder and libraries
    # Run model simulation
    java -cp app/netlogo-6.4.0.jar:. -Dnetlogo.extensions.dir=. ModelSim
done

csvstack ./outputs/raw_csv/*.csv > ./outputs/combined_data/Turtle_data_raw.csv