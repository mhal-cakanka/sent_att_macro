#!/bin/bash

nc=52
vers="5w"
depnum=1
indices=TRUE
estimtype='WLS3'

#for i in {1..404}
for i in {1..3}
do
   echo "Doing $i"
   echo "qsub -l select=1:ncpus=$nc:mem=200gb -l walltime=24:00:00 -- $HOME/sent_att_macro/model_flow.sh $i $nc $vers $depnum $indices $estimtype"
   qsub -l select=1:ncpus=$nc:mem=200gb -l walltime=24:00:00 -- $HOME/sent_att_macro/model_flow.sh $i $nc $vers $depnum $indices $estimtype
done
