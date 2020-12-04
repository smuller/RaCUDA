#!/bin/bash

for metric in divwarps shared global steps
do
    echo "----- METRIC $metric -----"
   for baseparam in "matrixMulBad matrixMulBad" "matrixMul matrixMul" "matrixMulTranspose matrixMul"
   do
       echo $baseparam
       ./runsim.sh $baseparam $metric "1261
1280
1281
1523
1600
1601
320
321
584
640
641
928
960
961"
   done	
   for baseparam in "vectorAdd vectoradd"
   do
       echo $baseparam
         ./runsim.sh $baseparam $metric "16777216
167772160
33554432
50331648
67108864
83886080
100663296
117440512
134217728
150994944"
   done
#    for baseparam in "histogram256 histogram"
#    do
#        echo $baseparam
#          ./runsim.sh $baseparam $metric "16777216
# 167772160
# 33554432
# 50331648
# 67108864
# 83886080
# 100663296
# 117440512
# 134217728
# 150994944
# 122992"
#      done
    for baseparam in "reduce0 reduce" "reduce1 reduce" "reduce2 reduce" "reduce3 reduce" "reduce2a reduce" "reduce3a reduce"
    do
        echo $baseparam
        ./runsim.sh $baseparam $metric "1024
131072
16384
2048
262144
32768
4096
65536
8192
"
    done

    for baseparam in "syn-brdis brdis" "syn-brdis-opt brdis"
    do
        echo $baseparam
        ./runsim2.sh $baseparam $metric "0-0 32-0 64-0 96-0 0-32 32-32 64-32 96-32 0-64 32-64 64-64 96-64 0-96 32-96 64-96 96-96 1-1 33-1 65-1 97-1 1-33 33-33 65-33 97-33 1-65 33-65 65-65 97-65 1-97 33-97 65-97 97-97 6-6 75-6 68-6 51-6 6-75 75-75 68-75 51-75 6-68 75-68 68-68 51-68 6-51 75-51 68-51 51-51"
    done

     for baseparam in "addSub0 addsub" "addSub1 addsub" "addSub2 addsub" "addSub3 addsub"
     do
         echo $baseparam
         ./runsim2.sh $baseparam $metric "1024-1024
1024-256
1024-512
1024-768
256-1024
256-256
256-512
256-768
512-1024
512-256
512-512
512-768
768-1024
768-256
768-512
768-768
258-258
258-514
258-770
514-258
514-514
514-770
770-258
770-514
770-770
584-584
584-880
584-890
880-584
880-880
880-890
890-584
890-880
890-890"
     done

#      echo "Mandelbrot_cuda mandelbrot"
#      ./runsim2.sh Mandelbrot_cuda mandelbrot $metric "1261-1024
# 1261-256
# 1261-512
# 1280-1024
# 1280-256
# 1280-512
# 1281-1024
# 1281-256
# 1281-512
# 1523-1024
# 1523-256
# 1523-512
# 1600-1024
# 1600-256
# 1600-512
# 1601-1024
# 1601-256
# 1601-512
# 320-1024
# 320-256
# 320-512
# 321-1024
# 321-256
# 321-512
# 584-1024
# 584-256
# 584-512
# 640-1024
# 640-256
# 640-512
# 641-1024
# 641-256
# 641-512
# 928-1024
# 928-256
# 928-512
# 960-1024
# 960-256
# 960-512
# 961-1024
# 961-256
# 961-512"
done
