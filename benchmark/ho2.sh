#!/bin/bash

echo -n > out1.txt

# for FILE in $(cat file_list/ho.txt)
while read -r FILE
do
  FILE=$(basename $FILE)
  echo $FILE
  echo $FILE "$(ls --full-time -l output/${FILE}__prover__1.in | cut -d " " -f 7)" \
    "$(ls --full-time -l output/${FILE}__prover__2.in | cut -d " " -f 7)" \
    "$(ls --full-time -l output/${FILE}__prover__3.in | cut -d " " -f 7)" >> out1.txt
done < file_list/ho.txt
