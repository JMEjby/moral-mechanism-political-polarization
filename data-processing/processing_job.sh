#!/bin/sh
# Grid Engine options (lines prefixed with #$)
#$ -N sims-processing
#$ -wd /exports/eddie/scratch/s1917169/moral-mechanism-political-polarization/data-processing
#$ -m beas
#$ -M s1917169@ed.ac.uk
#$ -l h_rt=100:00:00
#$ -l h_vmem=50G

# load R
module load R/4.4

# load python for pip install
module load anaconda

if ! command -v csvstack &> /dev/null; then
    echo "csvkit not found. Installing..."
    pip install csvkit
else
    echo "csvkit is already installed."
fi

export RUN_MODE=remote

Rscript 1-turtle_post_processing.R

Rscript 2-link_post_processing.R

csvstack ./intermediate-data/links/*.csv > ./intermediate-data/Link_data_raw.csv

Rscript 3-link_post_processing_2.R

Rscript 4-cluster_post_processing.R

Rscript 5-parquet_turtle_data.R