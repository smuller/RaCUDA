set terminal pdfcairo font "14pt"
set key left
set grid
set style line 1 lt 1 lw 0.1 pt 1 ps 0.5
set style line 2 lt 3 lw 1 pt 1 ps 2
set xlabel "w"
set ylabel "h"
set xtics 200
set ytics 200
set zlabel "sectors" offset -5,0,5
set lmargin 12
splot '../results/addSub1-results.tsv' using 1:2:3 title "Actual worst case" ls 1, 132*x*ceil(y/64) ls 2 title "Predicted worst case"
#set xlabel "#X"
#set ylabel "#Z" offset 0,0
#plot "../results/addSub1-results.tsv" skip 1 using 1:3 title "Actual worst case" ls 1, 132*x ls 2 #title "132w"
#set xlabel "#Y"
#set ylabel "#Z" offset 0,0
#plot "#FILE" using 2:3 title "Simulated worst case" ls 1, #MF ls 2 title "#HF"
