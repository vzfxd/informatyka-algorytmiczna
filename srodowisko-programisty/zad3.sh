#/bin/bash

words=$(find $1 -type f | xargs cat | tr -s '[:space:]' '\n' | sort | uniq)

for word in $words
do
    cnt=$(grep -wrl $word $1 | wc -l)
    echo "$cnt $word"
done