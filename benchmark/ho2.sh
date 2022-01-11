#!/bin/bash

if [ $# != 1 ]; then
  echo "Error: specify read file"
  exit 1
fi
LIST=$1

echo -n > out1.txt

# for FILE in $(cat file_list/ho.txt)
while read -r FILE
do
  FILE=$(basename $FILE)
  echo $FILE
  echo $FILE \
    "$(ls --full-time -l output/${FILE}__prover__1.in | cut -d " " -f 7)" \
    "$(ls --full-time -l output/${FILE}__prover__2.in | cut -d " " -f 7)" \
    "$(ls --full-time -l output/${FILE}__prover__3.in | cut -d " " -f 7)" \
    "$(ls --full-time -l output/${FILE}__prover__4.in | cut -d " " -f 7)" \
    "$(ls --full-time -l output/${FILE}__prover__5.in | cut -d " " -f 7)" \
    "$(ls --full-time -l output/${FILE}__prover__6.in | cut -d " " -f 7)" \
    "$(ls --full-time -l output/${FILE}__prover__7.in | cut -d " " -f 7)" \
    "$(ls --full-time -l output/${FILE}__prover__8.in | cut -d " " -f 7)" \
    "$(ls --full-time -l output/${FILE}__prover__9.in | cut -d " " -f 7)" \
      >> out1.txt
done < $LIST
