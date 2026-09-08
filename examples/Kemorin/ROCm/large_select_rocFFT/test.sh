#!/bin/bash
#PBS -P NIFS24KISC010
#PBS -q B_S
#####       Set number of nodes
#PBS -l select=1
#####       Set elapsed time
#PBS -l walltime=00:10:00

source ~/AMD_modules.sh
cd $PBS_O_WORKDIR

make
