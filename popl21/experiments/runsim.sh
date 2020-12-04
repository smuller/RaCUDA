#!/bin/bash
#Usage: <basename> <param> <metric> <values>
KERNEL_BASE=../kernels
PARAM_BASE=../params
ABSYNTH=../absynth-cuda

for n in $4
do
    if [ -f "$KERNEL_BASE/$1.cu" ]; then
	echo -n $n >> simresults/$1-$3.csv
	echo -n ", " >> simresults/$1-$3.csv
	sed "s/#1/$n/g" $PARAM_BASE/$2-temp > temp-params
	echo "$ABSYNTH -metric $3 -eval -param-file temp-params $KERNEL_BASE/$1.cu"
	$ABSYNTH -metric $3 -eval -param-file temp-params $KERNEL_BASE/$1.cu | sed -n 's/Evaluation cost: \([0-9]*\)/\1/p' >> simresults/$1-$3.csv
    else
	echo "Benchmark $1 not found -- it's probably one of the EULA-protected CUDA SDK benchmarks"
    fi
done
