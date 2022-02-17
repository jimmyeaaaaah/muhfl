#!/bin/bash

for ORGFILE in $(cat benchmark/file_list/all_paper.txt)
do
  FILE=benchmark/inputs/$ORGFILE
  echo "$FILE"
  timeout -s 9 60 dune exec bin2/evaluate_hflz.exe -- --try -1,1,2 --shortcircuit --max-expansion 10 "$FILE" > out_"$(basename "$FILE")".txt
  echo $? >> out_"$(basename "$FILE")".txt
done

echo "" > list.tmp
for FILE in $(find . -name "*.txt")
do

  echo $FILE $(tail -n 1 $FILE) >> list.tmp
done
