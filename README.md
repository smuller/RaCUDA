# RaCUDA
The artifact for Muller and Hoffmann's POPL 2021 paper "Modeling and Analyzing Evaluation Cost of CUDA Kernels"

RaCUDA is a static analysis tool for finding performance bottlenecks in
CUDA kernels. The tool is built on Absynth, a static resource analyzer
by Carbonneaux, Hoffmann, Ngo, and Wang. It comes with 
its rarely installed dependencies.  To build it you will still need to 
have the following core:
  
  * GMP 4.2 or up (with development headers)
  * MPFR 2.2 or up (with development headers)
  * GSL 1.6 or up (e.g., installed at /usr/local)
  * OCaml 4.0 or up (and ocamlbuild)
  * A C99 compiler (gcc/clang)
  * A C++ compiler (g++/clang++)

The location of most of these dependencies can be specified using
the main configure script, see ./configure -help for details.

This repository also contains, in the folder `popl21', the following
items specific to our POPL '21 paper:

  * `coq': Coq scripts
  * `curesults': Profiling results for GPU executions of kernels.
  * `experiments': Experiment scripts for reproducing the POPL '21
    results
  * `kernels': The CUDA kernels used as benchmarks in the evaluation.
    These may overlap with the example kernels in the top level of the
    repo, but are included here as a record of the benchmarks used in
    the POPL '21 evaluation
  * `model-eval': Spreadsheets of the results comparing the evaluation
    costs obtained by simulating the cost semantics in the paper with
    those obtained from profiling GPU executions.
  * `paper-analysis-results': Results from analyzing the kernels in our
    experiments.
  * `params': Parameter files for use in analyzing and simulating the
     benchmark kernels.
  * `plot': gnuplot files and plots for Figure 7 of the paper.
  * `simresults': Results from simulating execution of the kernels in
    our experiments.

## To build RaCUDA:

    ./configure
    make

The configuration script might fail to find the location of the GSL
library.  If that is the case, it will fail to build the `gsl_caller`
program.  This is however not fatal and many examples can be tested
without this program.  Similarly, `llvm-config' is not necessary for
running any of the CUDA examples and may be missing.

## To run RaCUDA

    absynth-cuda [-eval] -metric <metric> [-param-file <param file>] <kernel>

The above command runs the tool on the kernel <kernel>. The common options are:
  * `-metric <metric>' use the supplied resource metric. Supported metrics are:
    – divwarps: number of conditional instructions that cause a warp to diverge.
    – global: number of sectors read from/written to global memory.
    – shared: number of bank conflicts in shared memory. 
    – steps: count number of abstract instructions executed.
  * `-eval': run the tool in evaluation mode. In this mode, the tool does not
    perform a static resource analysis but rather simulates the execution of the
    kernel on a GPU and calculates the cost under th given resource metric using
    the cost model from our paper.
  * `-param-file <file>': load parameters from <file>. Parameters include block
    and grid size. For evaluation mode, initial values of any variables that
    affect control flow must be supplied. Parameter file is optional for
    analysis mode; default parameters will be used. Parameter files for each kernel are
available in popl20/params (do not use or modify parameter files ending in -temp; these are templates
used by the experiment scripts).

    source/absynth examples/probabilistic/linear/race.imp
    source/absynth -degree 2 examples/probabilistic/polynomial/trader.imp

## Examples

There are many examples (including the ones in our paper) in the
`kernels/` sub-directory.

Parameter files for the kernels used in our POPL '21 paper are available in
popl21/params (do not use or modify parameter files ending in -temp;
these are templates used by the experiment scripts).
