#!/bin/bash

echo 1
DATADIR=/storage/brno12-cerit/home/user123/sentiment

echo 2

export R_LIBS=/storage/brno12-cerit/home/user123/Rpackages

echo 3

echo "$PBS_JOBID is running on node `hostname -f`" >> $DATADIR/jobs_info.txt

echo 4

source /cvmfs/software.metacentrum.cz/modulefiles/5.1.0/loadmodules

echo 5

module add r/4.1.3-gcc-10.2.1-6xt26dl

echo 6

cd $DATADIR

echo 7

Rscript --vanilla hf_dataset.R $@ > $1.$2.Rout

echo 8
