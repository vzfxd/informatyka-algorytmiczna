#/bin/bash

IFS=$'\n'
files=$(find "$1" -type f)

for file in $files
do
    for line in $(cat $file)
    do
        words_uniq=$(echo $line | tr '[:space:]' '\n' | sort | uniq)
        for word in $words_uniq
        do  
            count=$(echo $line | grep -ow $word | wc -l)
            if [ "$count" -gt 1 ]; then
                echo "Plik: $file"
                echo "Wiersz: $line"
                echo "Słowo: $word"
                echo
            fi
        done
    done 
done