#/bin/bash

words=$(find $1 -type f | xargs cat | tr -s '[:space:]' '\n' | sort | uniq)

for word in $words
do
    echo $word
    grep -wr $word $1
    echo "=="
done