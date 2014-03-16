set terminal png
set output "test.png"
plot '<cat' using 1:2 with lines
