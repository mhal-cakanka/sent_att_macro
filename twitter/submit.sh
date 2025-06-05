#!/bin/bash

j=10000
for i in $(seq 1 $j 2182299);
do
        echo "Doing $i"
        echo "qsub -l select=1:ncpus=1:mem=10gb -l walltime=1:00:00 -- $HOME/sent_att_macro/tw_sent.sh $i $j"
        qsub -l select=1:ncpus=1:mem=10gb -l walltime=1:00:00 -- $HOME/sent_att_macro/tw_sent.sh $i $j
done