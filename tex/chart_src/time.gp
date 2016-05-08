set terminal pdf enhanced size 7,5 font "arial, 16"
set encoding iso_8859_2
set output '../chart/'.f.'_time.pdf'

set key off
set xlabel 'heuristika'
set ylabel 'čas (s)'
set title f.' problem'
set grid ytics
set boxwidth 0.6
set style fill solid 1.0
set xtics rotate
plot f using 1:4:xtic(2) with boxes lc rgb "#7AC36A"
