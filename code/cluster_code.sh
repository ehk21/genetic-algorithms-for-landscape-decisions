#!/bin/bash
#PBS -l walltime=72:00:00
#PBS -l select=1:ncpus=1:mem=5gb
cp -r $HOME/cluster $TMPDIR 
module load anaconda3/personal
echo "R is about to run"
R --vanilla < $HOME/cluster/code/run_optimisation_cluster.R 
mv cluster/results/optimisations/* $HOME/cluster/results/optimisations
echo "R has finished running"
# this is a comment at the end of the file

