set terminal pdfcairo font "14pt"
set key left
set grid
set style line 1 lt 1 lw 0.1 pt 1 ps 1
set style line 2 lt 2 lw 0.1 pt 2 ps 1
set style line 3 lt 3 lw 1 pt 1 ps 2
set xlabel "N"
set ylabel "conflicts"
set xrange [0:184549376]
set xtics ("" 0, "1x2^{24}" 16777216, "2x2^{24}" 33554432, "3x2^{24}" 50331648, "4x2^{24}" 67108864, "5x2^{24}" 83886080, "6x2^{24}" 100663296, "7x2^{24}" 117440512, "8x2^{24}" 134217728, "9x2^{24}" 150994944, "10x2^{24}" 167772160)
set lmargin 12
set rmargin 5
plot '../results/histogram-worst.tsv' using 1:3 title "Actual worst case" ls 1, '../results/histogram-rand.tsv' using 1:3 title "Random instance" ls 2, .875*(63+x)*2 ls 3 title "Predicted worst case"
#set xlabel "#X"
#set ylabel "#Z" offset 0,0
#plot "../results/addSub1-results.tsv" skip 1 using 1:3 title "Actual worst case" ls 1, 132*x ls 2 #title "132w"
#set xlabel "#Y"
#set ylabel "#Z" offset 0,0
#plot "#FILE" using 2:3 title "Simulated worst case" ls 1, #MF ls 2 title "#HF"
