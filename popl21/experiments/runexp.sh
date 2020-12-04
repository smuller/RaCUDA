#!/bin/bash

metrics='divwarps global shared steps'
ABSYNTH='../absynth-cuda'
PARAM_BASE=`pwd`'/../params/'
KERNEL_BASE=`pwd`'/../kernels/'
N=5

for m in $metrics
do
    rm results/$m.csv
    echo ""
    echo ""
    echo "------- STARTING METRIC $m --------"
    for i in $(cat experiments)
    do
	pf=`echo $i | awk -F: '{print $2}'`
        k=`echo $i | awk -F: '{print $1}'`
	if [ -f "$KERNEL_BASE$k" ]; then
            echo "$ABSYNTH -metric $m -param-file $PARAM_BASE$pf $KERNEL_BASE$k"
	    echo -n $k >> results/$m.csv
	    echo -n "," >> results/$m.csv

	    for j in $(seq 1 $N)
	    do
		$ABSYNTH -metric $m -param-file $PARAM_BASE$pf $KERNEL_BASE$k > temp
		echo -n `sed -n 's/.* runtime: \(.*\)s/\1/p' < temp` >> results/$m.csv
		echo -n "," >> results/$m.csv
	    done
	    echo -n "\"" >> results/$m.csv
	    echo -n `sed -n 's/.*Bound: \(.*\)/\1/p' < temp` >> results/$m.csv
	    echo "\"" >> results/$m.csv
	else
	    echo "Kernel $k not found -- it's probably one of the EULA-protected CUDA SDK benchmarks"
	fi
    done
done
