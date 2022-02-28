#!/bin/bash

if [ $# != 1 ]; then
  echo "Error: specify read file"
  exit 1
fi
LIST=$1

echo -n > out1.txt

while read -r FILE
do
  FILE=benchmark/inputs/$FILE
  
  dune exec bin2/reorder_arguments.exe $FILE
  res=$(diff ${FILE}_reorder.in ${FILE}_reorder_orig.in)
  if [ -n "$res" ]; then
    echo $FILE >> out1.txt
  fi
done < $LIST
