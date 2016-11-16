#!/usr/bin/gnuplot
#https://www.electricmonk.nl/log/2014/07/12/generating-good-looking-charts-with-gnuplot/


set terminal png size 2048,1024
set output "1.png"


# Thinner, filled bars
set boxwidth 0.4
set style fill solid 1.00

set title "Process Memory consumption in shutdown path"
set ylabel "Memory in GB"
# Show human-readable Y-axis. E.g. "100 k" instead of 100000
set format y '%.0s %c'


# Replace small stripes on the Y-axis with a horizontal gridlines
set tic scale 0
set grid ytics
# Remove border around chart
unset border

# Lighter grid lines
set grid ytics lc rgb "#C0C0C0"
set boxwidth 0.75
set style fill solid border -1
set xtics nomirror rotate by -55 scale 0.25



plot "OOMDATA.txt" using 2:xticlabels(1) with boxes lt rgb "#406090" title "Title",\
     "" using 3 lt rgb "#40FF00" title "Resident"
