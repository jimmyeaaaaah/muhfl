#!/bin/bash

# extra_termination
# extra_own
# termination_ho
BENCH_NAME=ho

echo '########## none ###########'
python3 bench1.py --timeout 900 --benchmark $BENCH_NAME muapprox_katsura --pass-args="--no-partial-analysis --no-usage-analysis"
cp output/0bench_out_append.txt_summary.txt 0bench_out_append.txt_summary_none.txt
bash ho2.sh file_list/${BENCH_NAME}.txt
cp out1.txt 0bench_out1_none.txt

echo '########## both ###########'
python3 bench1.py --timeout 900 --benchmark $BENCH_NAME muapprox_katsura
cp output/0bench_out_append.txt_summary.txt 0bench_out_append.txt_summary_both.txt 
bash ho2.sh file_list/${BENCH_NAME}.txt
cp out1.txt 0bench_out1_both.txt

echo '########## usage ###########'
python3 bench1.py --timeout 900 --benchmark $BENCH_NAME muapprox_katsura --pass-args="--no-partial-analysis"
cp output/0bench_out_append.txt_summary.txt 0bench_out_append.txt_summary_usage.txt
bash ho2.sh file_list/${BENCH_NAME}.txt
cp out1.txt 0bench_out1_usage.txt

echo '########## partial ###########'
python3 bench1.py --timeout 900 --benchmark $BENCH_NAME muapprox_katsura --pass-args="--no-usage-analysis"
cp output/0bench_out_append.txt_summary.txt 0bench_out_append.txt_summary_partial.txt
bash ho2.sh file_list/${BENCH_NAME}.txt
cp out1.txt 0bench_out1_partial.txt

echo '########## done ###########'
