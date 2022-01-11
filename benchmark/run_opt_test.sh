#!/bin/bash

SCRIPT_NAME=muapprox_katsura_no_options
BENCH_NAME=all_paper_3

echo '########## opt_noboth ###########'
python3 bench1.py --timeout 900 --benchmark $BENCH_NAME $SCRIPT_NAME
cp output/0bench_out_append.txt_summary.txt 0bench_out_append.txt_summary_opt_noboth.txt 
bash ho2.sh file_list/${BENCH_NAME}.txt
cp out1.txt 0bench_out1_opt_noboth.txt

echo '########## opt_agg ###########'
python3 bench1.py --timeout 900 --benchmark $BENCH_NAME $SCRIPT_NAME --pass-args="--agg"
cp output/0bench_out_append.txt_summary.txt 0bench_out_append.txt_summary_opt_agg.txt
bash ho2.sh file_list/${BENCH_NAME}.txt
cp out1.txt 0bench_out1_opt_agg.txt

echo '########## no_rd ###########'
python3 bench1.py --timeout 900 --benchmark $BENCH_NAME $SCRIPT_NAME --pass-args="--remove-disjunction"
cp output/0bench_out_append.txt_summary.txt 0bench_out_append.txt_summary_opt_no_rd.txt
bash ho2.sh file_list/${BENCH_NAME}.txt
cp out1.txt 0bench_out1_opt_no_rd.txt

echo '########## both ###########'
python3 bench1.py --timeout 900 --benchmark $BENCH_NAME $SCRIPT_NAME --pass-args="--agg --remove-disjunction"
cp output/0bench_out_append.txt_summary.txt 0bench_out_append.txt_summary_opt_both.txt
bash ho2.sh file_list/${BENCH_NAME}.txt
cp out1.txt 0bench_out1_opt_both.txt

echo '########## done ###########'
