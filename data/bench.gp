set term png enhanced font "Vera,10"
set output "bench.png"
set title "grep vs hsgrep"
set xlabel "file size [bytes]"
set ylabel "secs"
set key inside right bottom
plot "bench-grep.txt" with linespoints title "grep", "bench-bytestring.txt" with linespoints title "hsgrep", "bench-grep.txt" using 1:(log($1)/460) with linespoints title "K log(x)"
