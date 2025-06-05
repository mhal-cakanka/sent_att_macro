#!/bin/bash

NS=504
step=20

for i in $(seq 21 $step $NS)
do
	echo "Doing loop $i"
	from=$i
	# to=$i+19
        to=$((i + 19))
	
	if (( to > NS ))
	then
		to=$NS
		echo "Fixed TO to NS"
	fi
	echo "Doing $from $to"
	echo "qsub -l select=1:ncpus=1:mem=10gb -l walltime=4:00:00 -- $HOME/sentiment/hf_dataset.sh $from $to"
	qsub -l select=1:ncpus=1:mem=10gb -l walltime=4:00:00 -- $HOME/sentiment/hf_dataset.sh $from $to
done

