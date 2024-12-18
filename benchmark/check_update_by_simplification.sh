#!/bin/bash

for ORGFILE in $(cat benchmark/file_list/all_paper.txt)
do
  FILE=benchmark/inputs/$ORGFILE
  echo "$FILE"
  dune exec bin2/simplify.exe -- --output-cp --show-style abbrev --agg "$FILE" > out1.txt
  res=$(diff "$FILE" "$FILE"_cp.in)
  if [ "$res" ]; then
    echo "CHANGED: $FILE"
  fi
done
